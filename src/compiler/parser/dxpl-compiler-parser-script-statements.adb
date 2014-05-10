separate (DXPL.Compiler.Parser.Script)

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/018/2009

--!  Purpose:
--!  Statements like 'A := B + C;' are handled, such that
--!  for each statement, a syntax tree is build. Each statement,
--!  which contains an addition, can be analyzed. To analyze
--!  statements, you have to provide the annotation '--# BEGIN'.
--!
--!  Blocks of statements, which are surrounded by '--# BEGIN'
--!  and '--# END', will be analyzed. Please specify these
--!  annotations always as pairs.
----------------------------------------------------------------
procedure Statements (Annotation_Object : in out Annotation_Manager.Instance)is

	procedure Expression (Annotation_Object : in Annotation_Manager.Instance) is separate;
	--  Expression ::=
	--    <identifier> [ "(" <digit> ")" ] := <expression> ";"

	procedure Procedure_Call is separate;
	--  Procedure_Call ::=
	--    <identifier> "(" <expression> { "," <expression> } ")"
	
	procedure Loop_Unrolling is
	--  Supports unrolling of loops by means of internal memory.

		Replace_Id : Unbounded_String := Lexer.Current_Symbol.Value;
		From, To   : Integer := 0;
		Elements   : Lexer.Lexical_Vector.Vector;
	begin
		--  for <identifier> <numeric> ".." <numeric> loop
		Match (Z_IDENTIFIER);
		Match (R_IN);
		if Lexer.Current_Symbol.Lex_Element /= L_NUMERIC then
			Error (Loop_Exception'Identity,
			  "Loop declaration in line" & 
			  Positive'Image (Lexer.Current_Symbol.Line) & " is not correct. " &
			  "Declarations of 'for' loops are currently only allowed " &
			  "using integers, e.g. 'for I in 1 .. 2'.");
		end if;
		From := Integer'Value (To_String (Lexer.Current_Symbol.Value));
		Match (L_NUMERIC);
		Match (D_DOT);
		Match (D_DOT);
		if Lexer.Current_Symbol.Lex_Element /= L_NUMERIC then
			Error (Loop_Exception'Identity,
			  "Loop declaration in line" & 
			  Positive'Image (Lexer.Current_Symbol.Line) & " is not correct. " &
			  "Declarations of 'for' loops are currently only allowed " &
			  "using integers, e.g. 'for I in 1 .. 2'.");
		end if;
		To := Integer'Value (To_String (Lexer.Current_Symbol.Value));
		Match (L_NUMERIC);
		Match (R_LOOP);

		if (To - From) > Iteration_Threshold then
		--  Limit the loop unrolling algorithm to a maximum of 100 iterations
		--  TODO: check the magnitude? |(to - from)|
			Error (Loop_Out_of_Range'Identity, "There are more iterations than allowed in the " & 
				"declaration of the loop in line" & Positive'Image (Lexer.Current_Symbol.Line) & 
				". Loop unrolling will not be performed. Please revise your code.");
		end if;

		loop  --  read tokens into memory
			exit when Lexer.Current_Symbol.Lex_Element = R_END;
			Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
			Next_Symbol;
		end loop;
		Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
		Input_Manager.Add (Replace_Id, Elements);

		--  switch to internal memory
		Input_Manager.Set_Input (MEMORY, Replace_Id);

		declare  --  start memory-based unrolling
			Loop_Unwinding : Lexer.Lexical_Vector.Vector;
		begin
			for Index in From .. To loop
				if Input_Manager.Get_Input = MEMORY then
					Input_Manager.Reset;
				end if;

				loop
					exit when Lexer.Current_Symbol.Lex_Element = R_END;
					if Lexer.Current_Symbol.Value = Replace_Id then
					--  replace the loop index with the current number of iteration
						Lexer.Current_Symbol.Value       := To_Unbounded_String (Integer'Image (Index));
						Lexer.Current_Symbol.Lex_Element := L_NUMERIC;
						Lexer.Current_Symbol.Lex_Type    := TYPE_NUMERIC_LITERAL;
					end if;
					Lexer.Lexical_Vector.Append (Loop_Unwinding, Lexer.Current_Symbol);
					Next_Symbol;
				end loop;
			end loop;

			Lexer.Lexical_Vector.Append (Loop_Unwinding, Lexer.Current_Symbol);
			Input_Manager.Replace_Dataset (Loop_Unwinding);
		end;

		--  handle common statements. 
		--  remind, that we are still reading from memory
		declare
			Annotation_Object : Annotation_Manager.Instance;
		begin
			Statements (Annotation_Object);
		end;

		--  reset to FILE
		Input_Manager.Set_Input (FILE);

		--  end loop
		Match (R_END);
		Match (R_LOOP);
	end Loop_Unrolling;

	procedure Declare_Block (Annotation_Object : in Annotation_Manager.Instance) is separate;
	--  item ::=
	--    <digit> "=>" <digit>
	--
	--  element ::=
	--    <digit> | <identifier> | "(" ( others "=>" ( <digit> | <identifier> )) | <item> { "," <item> } ")" 
	--
	--  variables_declaration ::=
	--    <identifier> { "," <identifier> } ":" [ constant ] <identifier> [ ":=" <element> ] ";"
	--
	--  declare_block ::=
	--    declare <variables_declaration> begin <statements> end ";" 

begin
	case Lexer.Current_Symbol.Lex_Element is
		when R_NULL =>
		--  Listed for compatibility

			Trace (Process_Procedure.Get_Next_Declaration);
			Match_and_Trace (R_NULL);
			Trace ("end");
			Match_and_Trace (D_SEMI_COLON);
			Trace (" " & ASCII.CR);

		when Z_IDENTIFIER =>
		--  When we read in an identifier, we have to differentiate
		--  between identifiers, that initiate a procedure call or
		--  ones defining an expression. If an identifier is
		--  indeed a function call .. error!

			if Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function) then
				Error (Not_Implemented'Identity,
					"Direct function calls are currently not supported. Error caused " & 
					"in line " & Positive'Image (Lexer.Current_Symbol.Line) &
					" with identifier '" & To_String (Lexer.Current_Symbol.Value) & "'.");

			elsif Symbol_Table.Has_Element (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_procedure) then
				Procedure_Call;
				Trace ("end");
				Match_and_Trace (D_SEMI_COLON);

			else
				Trace (Process_Procedure.Get_Next_Declaration);
				Expression (Annotation_Object);
				Trace ("end;");
			end if;
			
			Trace (" " & ASCII.CR);

		when R_FOR =>
		--  Handle for loops to proceed loop unrolling.

			Match (R_FOR);
			Loop_Unrolling;
			Match (D_SEMI_COLON);

		when R_DECLARE =>
		--  Support declare statements. Each declaration
		--  is encapsulated in an autonomous procedure.

			Match (R_DECLARE);
			loop
				exit when Lexer.Current_Symbol.Lex_Element = R_BEGIN;
				Trace (Process_Procedure.Get_Next_Declaration);
				Declare_Block (Annotation_Object);
				Trace ("end;");
			end loop;
			Match (R_BEGIN);
			Statements (Annotation_Object);
			Match (R_END);
			Match (D_SEMI_COLON);

		when Z_ANNOTATION =>
		--  Handle annotations like '--# BEGIN' and '--# END'
		--  to allow explicit declaration of regions, which
		--  shall be analyzed by darXplorer. Defining two 
		--  consecutive 'BEGIN' or 'END' statements are treated
		--  as errors. Also declaring 'END' without a 'BEGIN'
		--  annotation or vice versa is an error.

			Annotation_Manager.Update (Annotation_Object, 
									   Lexer.Current_Symbol.Value, 
									   Lexer.Current_Symbol.Column,
									   Lexer.Current_Symbol.Line,
									   Error'Access);
			Match (Z_ANNOTATION);

		when R_END =>
		--  No more statements are to be expected. For the last time,
		--  check, if an 'END' annotation is missing.

			if Annotation_Manager.Is_End_Missing (Annotation_Object) then
				Error (Annotation_Manager.Missing_Annotation'Identity,
					"The 'END' annotation is missing. " &
					"See line" & Positive'Image (Lexer.Current_Symbol.Line) & ".");
			end if;
			return;

		when others =>
		--  This token is currently not supported by darXplorer.
			Error (Not_Implemented'Identity,
				"Your declared element '" &
				To_String (Lexer.Current_Symbol.Value) &
				" ' in line" & Positive'Image (Lexer.Current_Symbol.Line) &
				" is currently not supported.");
	end case;

	--  recursive call
	Statements (Annotation_Object);
end Statements;
