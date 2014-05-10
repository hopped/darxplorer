separate (DXPL.Compiler.Parser.Script)

procedure With_Declaration is
--  with_declaration ::= 
--    with <identifier> { "." <identifier> } ";"

	procedure Handle_Word_Size is
	--  Determines the current word size, e.g. 32 or 64 bit
	begin
		Ada.Text_IO.Set_Output (File => File_DXPL);

		Trace ("with Interfaces; ");
		Trace ("package DXPL is ");
		if Count (Lexer.Current_Symbol.Value, "32") > 0 then
			Trace ("type Word is new Interfaces.Unsigned_32; ");
		else
			Trace ("type Word is new Interfaces.Unsigned_64; ");
		end if;

		Ada.Text_IO.Set_Output (File => File_Round);
	end Handle_Word_Size;

begin
	Match (R_WITH);
	if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
		Handle_Word_Size;
	
		Match (Z_IDENTIFIER);
		Match (D_SEMI_COLON);
	else
		Trace ("with ");
		Match_and_Trace (Z_IDENTIFIER);
		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_SEMI_COLON;
			Match_and_Trace (D_DOT);
			Match_and_Trace (Z_IDENTIFIER);
		end loop;
	end if;
end With_Declaration;