separate (DXPL.Compiler.Parser)

--  expression ::=
--  <identifier> [ "(" <digit> ")" ] "+" | "-" | xor | or | and | mod <identifier> [ "(" <digit> ")" ] ";" |
--  <identifier> "(" <variable> "," <digit> ")" ";"

--  element ::=
--  <simple_expression> [ [ "=" | "/=" | "<" | "<=" | ">" | ">=" ] <simple_expression> ] [ [ and | or | xor ] <simple_expression> ]
--
--  expression ::= <element> { <element> }
function Expression return Binary_Tree is
	Tree : Binary_Tree;
begin
	Debug.Trace ("EXPRESSION ");
	
	-- <simple_expression>
	Tree := Simple_Expression;

	-- [ "=" | "/=" | "<" | "<=" | ">" | ">=" ] <simple_expression>
	case Lexer.Current_Symbol.Lex_Element is
		when D_EQUALS_SIGN =>
			Next_Symbol;
			
		when C_INEQUALITY =>
			Next_Symbol;
			
		when D_LESS_THAN =>
			Next_Symbol;
			
		when C_LESS_THAN_OR_EQUAL =>
			Next_Symbol;
			
		when D_GREATER_THAN =>
			Next_Symbol;
			
		when C_GREATER_THAN_OR_EQUAL =>
			Next_Symbol;
			
		when C_NAMED =>
			Insert (Tree, Lexer.Current_Symbol.Value, Left);
			Match (C_NAMED);
			
			declare
				Tree_2 : Binary_Tree := Simple_Expression;
			begin
				Swap_Child (Tree, Tree_2, Right);
			end;
			
		when others =>
			null;
	end case;
	
	-- [ and | or | xor ] <simple_expression>
	if Lexer.Current_Symbol.Lex_Type = TYPE_RESERVED_WORD then
		Next_Symbol;
	end if;
		
	return Tree;
end Expression;