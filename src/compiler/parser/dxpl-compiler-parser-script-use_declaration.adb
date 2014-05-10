separate (DXPL.Compiler.Parser.Script)

procedure Use_Declaration is
--  use-declaration ::= 
--    use <identifier> { "." <identifier> } ";"
begin
	Match (R_USE);
	if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
		Match (Z_IDENTIFIER);
		Match (D_SEMI_COLON);
	else
		Trace ("use ");
		Match_and_Trace (Z_IDENTIFIER);
		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_SEMI_COLON;
			Match_and_Trace (D_DOT);
			Match_and_Trace (Z_IDENTIFIER);
		end loop;
	end if;
end Use_Declaration;