separate (DXPL.Compiler.Parser)

--  primary ::=
--  "(" <expression> ")" | <identifier> [ "(" <expression> { "," <expression> } ")" ]
function Primary return Binary_Tree is
	Tree : Binary_Tree;
begin
	Debug.Trace ("PRIMARY with " & To_String (Lexer.Current_Symbol.Value));
	
	-- "(" <expression> ")" 
	if Lexer.Current_Symbol.Lex_Element = D_LEFT_PARENTHESIS then
		Match (D_LEFT_PARENTHESIS);
		Tree := Expression;
		Match (D_RIGHT_PARENTHESIS);
	
	elsif Lexer.Current_Symbol.Lex_Type = TYPE_NUMERIC_LITERAL then
		declare
			From : Unbounded_String := Lexer.Current_Symbol.Value;
		begin
			Match (L_NUMERIC);
			if Lexer.Current_Symbol.Lex_Element = D_DOT then
				Match_and_Append (D_DOT, From);
				Match_and_Append (D_DOT, From);
				Match_and_Append (L_NUMERIC, From);
			end if;
			
			Insert (Tree, From, Left);
		end;
	
	elsif Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
		Match (Z_IDENTIFIER);
		Match (D_DOT);
		
		if Lexer.Current_Symbol.Lex_Element = Z_IDENTIFIER and then Lexer.Current_Symbol.Value = "Rounds" then
			Insert (Tree, To_Unbounded_String ("State.Round_Offset "), Left);
			Next_Symbol;
		end if;
			
	--  named expression like others => 16#0#
	elsif Lexer.Current_Symbol.Lex_Type = TYPE_RESERVED_WORD and then Lexer.Current_Symbol.Lex_Element = R_OTHERS then
		Insert (Tree, Lexer.Current_Symbol.Value, Left);
		Match (R_OTHERS);
	
	else
		declare
			Identifier    : Unbounded_String := Null_Unbounded_String;
			Function_Name : Unbounded_String := Null_Unbounded_String;
		begin
			if not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_variable) and
			   not Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function) then
				Identifier := To_Unbounded_String ("State.");
			end if;
			Function_Name := Lexer.Current_Symbol.Value;
			Match_and_Append (Z_IDENTIFIER, Identifier);

			if Symbol_Table.Has_Element (S_Table, Function_Name, Symbol_Table.t_function) then
				if Lexer.Current_Symbol.Lex_Element = D_LEFT_PARENTHESIS then
					Match_and_Append (D_LEFT_PARENTHESIS, Identifier);
					
					declare
						Tree : Binary_Tree := Expression;
					begin
						Append (Source => Identifier, New_Item => Syntax_Tree.Get_Infix_Statement (Tree, False, True));
						
						while Lexer.Current_Symbol.Lex_Element = D_COMMA loop
							Match_and_Append (D_COMMA, Identifier);
							Tree := Expression;
							Append (Source => Identifier, New_Item => Syntax_Tree.Get_Infix_Statement (Tree, False, True));
						end loop;
					end;
					--  ")"
					Match_and_Append (D_RIGHT_PARENTHESIS, Identifier);
				end if;
			end if;
			
			if Lexer.Current_Symbol.Lex_Element = D_LEFT_PARENTHESIS then
				--  "("
				Match_and_Append (D_LEFT_PARENTHESIS, Identifier);
				declare
					--  <expression>
					Tree : Binary_Tree := Expression;
				begin
					Append (Source => Identifier, New_Item => Syntax_Tree.Get_Infix_Statement (Tree, False, True));
				end;

				--  { <expression> "," } <expression>
				while Lexer.Current_Symbol.Lex_Element = D_COMMA loop
				--  multi-dimensional array detected
					Match_and_Append (D_COMMA, Identifier);
					declare
						Tree : Binary_Tree := Expression;
					begin
						Append (Source => Identifier, New_Item => Syntax_Tree.Get_Infix_Statement (Tree, False, True));
					end;
				end loop;

				--  ")"
				Match_and_Append (D_RIGHT_PARENTHESIS, Identifier);

			end if;
			Insert (Tree, Identifier, Left);
		end;
	end if;

	return Tree;
end Primary;
