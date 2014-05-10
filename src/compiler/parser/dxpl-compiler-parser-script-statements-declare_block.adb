separate (DXPL.Compiler.Parser.Script.Statements)

procedure Declare_Block (Annotation_Object : in Annotation_Manager.Instance) is
--  parameter ::=
--  <identifier> { "," <identifier } ":" [ in | out | in out ] <identifier> [ := <expression> ] ";"
	
	Key, Item     : Unbounded_String := Null_Unbounded_String;
	Elements      : Lexer.Lexical_Vector.Vector;
begin
	Key := Lexer.Current_Symbol.Value;
	
	--  <identifier> { "," <identifier> }
	Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
	Match_and_Append (Z_IDENTIFIER, Item);
	loop
		exit when Lexer.Current_Symbol.Lex_Element = D_COLON;
		Match_and_Append (D_COMMA, Item);
		Symbol_Table.Push (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_variable);
		Match_and_Append (Z_IDENTIFIER, Item);
	end loop;

	--  ":" [ constant ]
	Match_and_Append (D_COLON, Item);
	Append (Source => Item, New_Item => "aliased ");
	if Lexer.Current_Symbol.Lex_Element = R_CONSTANT then
		Match_and_Append (R_CONSTANT, Item);
	end if;
	
	--  <identifier>
	if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
		Match_Lenient (Z_IDENTIFIER);
		Match_Lenient (D_DOT);
	end if;
	Match_and_Append (Z_IDENTIFIER, Item);

	--  [ := <expression> ]
	if Lexer.Current_Symbol.Lex_Element = C_ASSIGNMENT then
		Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
		Match (C_ASSIGNMENT);
		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_SEMI_COLON;
			Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
			Next_Symbol;
		end loop;
		Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
		
		--  store vector in map
		Input_Manager.Add (Key, Elements);

		Input_Manager.Set_Input (MEMORY, Key);

		Expression (Annotation_Object);

		Input_Manager.Set_Input (FILE);
	end if;
		
	Match_and_Append (D_SEMI_COLON, Item);
	
	--  keep track of variables, that are part of the round state
	Round_State.Add (R_State, Key, Item);
end Declare_Block;
