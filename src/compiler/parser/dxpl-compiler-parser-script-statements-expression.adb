separate (DXPL.Compiler.Parser.Script.Statements)

procedure Expression (Annotation_Object : in Annotation_Manager.Instance) is
	--  Expression ::=
	--    <identifier> [ "(" <digit> ")" ] := <expression> ";"

	procedure Append_Differential_Probability (New_Statement : in     Sum_Type;
											   Line          : in out Ada.Strings.Unbounded.Unbounded_String) is

	begin
		Append (Source => Line, New_Item => "Analyse ( ");
		Append (Source => Line, New_Item => New_Statement.Left_Addend  & ", " & 
											New_Statement.Right_Addend & ", " & 
											New_Statement.Sum & ", ");
		Append (Source => Line, New_Item => "State.Output_Candidates, State.Local_Probability);" );
	end Append_Differential_Probability;

	procedure Append_Return (New_Statement : in     Sum_Type;
							 Line          : in out Ada.Strings.Unbounded.Unbounded_String) is

	begin
		Append (Source => Line, New_Item => "return " & New_Statement.Sum & "'Access; ");
	end Append_Return;

	Operand_Tree  : Binary_Trees.Binary_Tree;
	Line          : Unbounded_String := Null_Unbounded_String;
	New_Statement : aliased Sum_Type;
begin
	Debug.Trace ("MATH_EXPRESSION");

	if not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_variable) and 
	   not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function) then
		Append (Source => New_Statement.Sum, New_Item => "State.");
	end if;
	Match_and_Append (Z_IDENTIFIER, New_Statement.Sum);
	if Lexer.Current_Symbol.Lex_Element = D_LEFT_PARENTHESIS then
		Match_and_Append (D_LEFT_PARENTHESIS,  New_Statement.Sum);
		declare
			Tree : Binary_Tree := Expression;
		begin
			Append (Source => New_Statement.Sum, New_Item => To_Unbounded_String (Syntax_Tree.Get_Infix_Statement (Tree, False, True)));
		end;
		Match_and_Append (D_RIGHT_PARENTHESIS, New_Statement.Sum);
	end if;
	Append (Source => Line, New_Item => New_Statement.Sum);
	Match (C_ASSIGNMENT);

	-- AST, set ':=' as root element
	Clear  (Operand_Tree);
	Insert (Operand_Tree, To_Unbounded_String (":="), Left);
	Append (Operand_Tree, New_Statement.Sum, Left, Left);

	declare
		Tree : Binary_Tree := Expression;
		pragma Warnings (Off, Tree);
	begin
		Swap_Child (Operand_Tree, Tree, Right);
	end;

	if Annotation_Manager.Discard_Analyzation (Annotation_Object) then
		Trace (Syntax_Tree.Get_Infix_Statement (Operand_Tree, True));
		Match (D_SEMI_COLON);
		if Lexer.Current_Symbol.Lex_Element = Z_IDENTIFIER then
			if not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function) and
			   not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_procedure) then
				Expression (Annotation_Object);
			end if;
		else
			Trace ("State.Local_Probability := Conditioned_Float'Last; ");
			Trace ("return null; ");
		end if;
		return;
	end if;

	declare
		Count_Plus, Depth : Natural := 0;
	begin
		Count_Addition (Operand_Tree, Count_Plus, Depth);

		-- iterative addition
		if Count_Plus >= 1 and Depth > 2 then
			declare
				Root_Tree  : Binary_Tree := Create (Operand_Tree);
				Expression : Statement_Vector.Vector;
				Cursor     : Statement_Vector.Cursor;
			begin
				Filter_Addition (Operand_Tree, Expression);
				Cursor := Statement_Vector.First (Expression);

				while Statement_Vector.Has_Element (Cursor) loop
					Line := Null_Unbounded_String;
					-- analyse statement
					Append_Differential_Probability (Statement_Vector.Element (Cursor), Line);
					-- return statement
					Append_Return (Statement_Vector.Element (Cursor), Line);
					Trace (To_String (Line));

					Statement_Vector.Next (Cursor);

					Trace ("end; ");
					Trace (Process_Procedure.Get_Next_Declaration);
				end loop;

				Trace (Syntax_Tree.Get_Infix_Statement (Root_Tree, True));
				Trace ("State.Local_Probability := Conditioned_Float'Last; ");
				Trace ("return null; ");
				Match (D_SEMI_COLON);
			end;

		--  single addition
		elsif Count_Plus = 1 and Depth = 2 then
			declare
				Single_Statement : Sum_Type := Split_Addition (Operand_Tree);
			begin
				if not (Single_Statement.Left_Addend = Null_Unbounded_String) then
					Single_Statement.Sum := New_Statement.Sum;
					Right_Child (Operand_Tree);
					Single_Statement.Left_Addend  := To_Unbounded_String (Syntax_Tree.Get_Infix_Statement (Left_Child (Operand_Tree),  False, True));
					Single_Statement.Right_Addend := To_Unbounded_String (Syntax_Tree.Get_Infix_Statement (Right_Child (Operand_Tree), False, True));

					Line := Null_Unbounded_String;
					-- analyse statement
					Append_Differential_Probability (Single_Statement, Line);
					-- return statement
					Append_Return (Single_Statement, Line);

					Trace (To_String (Line));
					Match (D_SEMI_COLON);
				else
					Trace (Syntax_Tree.Get_Infix_Statement (Operand_Tree, True));
					Match (D_SEMI_COLON);
					if Lexer.Current_Symbol.Lex_Element = Z_IDENTIFIER then
						if not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function) and
						   not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_procedure) then
							Expression (Annotation_Object);
						end if;
					else
						Trace ("State.Local_Probability := Conditioned_Float'Last; ");
						Trace ("return null; ");
					end if;
				end if;
			end;

		--  a binary operation unlike addition
		else
			Trace (Syntax_Tree.Get_Infix_Statement (Operand_Tree, True));
			Match (D_SEMI_COLON);
			if Lexer.Current_Symbol.Lex_Element = Z_IDENTIFIER then
				if not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function) and
				   not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_procedure) then
					Expression (Annotation_Object);
				end if;
			else
				Trace ("State.Local_Probability := Conditioned_Float'Last; ");
				Trace ("return null; ");
			end if;
		end if;
	end;
	Match_Lenient (D_SEMI_COLON);
end Expression;
