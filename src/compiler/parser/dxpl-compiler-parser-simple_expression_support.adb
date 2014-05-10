separate (DXPL.Compiler.Parser)

--  primary ::=
--  "(" <expression> ")" | <identifier> [ "(" <expression> { "," <expression> } ")" ]
--
--  <Parse_Expression> ::= 
--  <primary> [ "**" <primary> ] [ "*" | mod | "/" ] [ "+" | "-" ]
function Simple_Expression_Support return Binary_Trees.Binary_Tree is
	Operator : Unbounded_String := Null_Unbounded_String;
	Tree, Tree_1, Tree_2, Root : Binary_Trees.Binary_tree;
	Success : Boolean := False;
begin
	Debug.Trace ("SIMPLE_EXPRESSION_SUPPORT ");
	
	--  <primary>
	Tree_1 := Parse_Expression;

	--  [ "**" <primary> ]
	if Lexer.Current_Symbol.Lex_Element = C_EXPONENTIATE then
		Match (C_EXPONENTIATE);
	end if;

	while Is_Operator loop
		Operator := Lexer.Current_Symbol.Value;
		Next_Symbol;

		Tree_2 := Parse_Expression;

		if Success then
			declare
				Tree_3 : Binary_Tree := Create (Tree);
			begin
				Insert_Root (Tree, Operator, Left);
				Swap_Child (Tree, Tree_3, Left);
				Swap_Child (Tree, Tree_2, Right);
				Root := Tree;
			end;
		else
			Insert     (Tree, Operator, Left);
			Root := Tree;
			Swap_Child (Tree, Tree_1,   Left);
			Swap_Child (Tree, Tree_2,   Right);
		end if;

		Success := True;
	end loop;

	if Success then
		return Root;
	else
		return Tree_1;
	end if;
end Simple_Expression_Support;