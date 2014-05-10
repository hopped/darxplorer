separate (DXPL.Compiler.Parser.Script)

procedure Function_Declaration is
--  function_declaration ::=
--    function <identifier> is | function <identifier> 
--    "(" <function_parameter_declaration> ")" is

	procedure Inner_Loop is
	--  loops until 'end' is reached
	begin
		loop
			exit when Lexer.Current_Symbol.Lex_Element = R_END;
			if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
				Match (Z_IDENTIFIER);
				Match (D_DOT);
			end if;

			if Round_State.Contains (R_State, Lexer.Current_Symbol.Value) then
				Trace ("State.");
			end if;

			if Lexer.Current_Symbol.Lex_Element = Z_ANNOTATION then
				Error (Annotation_Manager.Wrong_Nesting_Of_Annotations'Identity,
				  "Annotations are not allowed in functions. Error occured in " &
				  "line" & Positive'Image (Lexer.Current_Symbol.Line) & ".");
			end if;
			Trace;
			Next_Symbol;
		end loop;
	end Inner_Loop;

begin
	Match_and_Trace (R_FUNCTION);
	Symbol_Table.Push (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_function);
	
	Inner_Loop;
	Match_and_Trace (R_END);
	loop
		exit when not (Lexer.Current_Symbol.Lex_Element = R_LOOP);
		Inner_Loop;
		Match_and_Trace (R_END);
	end loop;

	Match_Lenient (Z_IDENTIFIER);
	Match_and_Trace (D_SEMI_COLON);
end Function_Declaration;
