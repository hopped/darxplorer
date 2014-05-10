separate (DXPL.Compiler.Parser)

function Simple_Expression return Binary_Tree is
--  primary ::=
--  "(" <expression> ")" | <identifier> [ "(" <expression> { "," <expression> } ")" ]
--
--  simple_expression_support ::= 
--  <primary> [ "**" <primary> ] [ "*" | mod | "/" ] [ "+" | "-" ]
--
--  simple_expression ::=
--  [ "+" | "-" | "not" ] <simple_expression_support> { <simple_expression_support> }

	Tree : Binary_Tree;
begin
	Debug.Trace ("SIMPLE_EXPRESSION ");

	--  [ "+" | "-" | not ]
	case Lexer.Current_Symbol.Lex_Element is
		when D_PLUS =>
			Insert (Tree, Lexer.Current_Symbol.Value, Left);
			Next_Symbol;

		when D_MINUS =>
			Insert (Tree, Lexer.Current_Symbol.Value, Left);
			Next_Symbol;

		when R_NOT =>
			Insert (Tree, Lexer.Current_Symbol.Value, Left);
			Next_Symbol;

		when others =>
			null;
	end case;

	--  <simple_expression_support> { <simple_expression_support> }
	Tree := Simple_Expression_Support_0;

	return Tree;
end Simple_Expression;