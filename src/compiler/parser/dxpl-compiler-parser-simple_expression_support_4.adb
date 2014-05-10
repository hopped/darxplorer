with Ada.Strings.Maps.Constants;

separate (DXPL.Compiler.Parser)

function Simple_Expression_Support_4 return Binary_Tree is

	function Is_Interface_Function (Item : in Unbounded_String) return Boolean is
		use Ada.Strings.Maps;
		Element : Unbounded_String := Translate (Item, Ada.Strings.Maps.Constants.Lower_Case_Map);
	begin
		Element := Trim (Element, Ada.Strings.Right);
		if    Element = "rotate_right" then
			return True;
		elsif Element = "rotate_left"  then
			return True;
		elsif Element = "shift_left"   then
			return True;
		elsif Element = "shift_right"  then
			return True;
		end if;
			
		return False;
	end Is_Interface_Function;

	Operator : Unbounded_String := Null_Unbounded_String;
	Tree, Tree_1, Root : Binary_Trees.Binary_tree;
	Success : Boolean := False;
begin
	Debug.Trace ("SIMPLE_EXPRESSION_SUPPORT_4 ");

	--  <primary>
	Tree_1 := Primary;

	if Lexer.Current_Symbol.Lex_Element = Z_IDENTIFIER and then Is_Interface_Function (Lexer.Current_Symbol.Value) then
		Operator := Lexer.Current_Symbol.Value; --  identifier = operator
		Next_Symbol;
		
		declare
			Tree_1, Tree_2 : Binary_Trees.Binary_Tree;
		begin
			Match (D_LEFT_PARENTHESIS);
			Tree_1 := Expression;
			Match (D_COMMA);
		
			case Lexer.Current_Symbol.Lex_Element is
				when L_NUMERIC =>
					Tree_2 := Primary;
					
				when Z_IDENTIFIER =>
					declare
						Function_Name : Unbounded_String := Lexer.Current_Symbol.Value;
					begin
						Tree_2 := Primary;
						if Lexer.Current_Symbol.Lex_Element = D_LEFT_PARENTHESIS then
						--  it's a function call
							Match (D_LEFT_PARENTHESIS);
							Tree_2 := Expression;
							Insert (Tree_2, Function_Name, Right);
							Match (D_RIGHT_PARENTHESIS);
						end if;
					end;
					
				when others =>
					Error (Not_Implemented'Identity,
					  "We currently do not support calling '" &
					  To_String (Lexer.Current_Symbol.Value) &
					  "' in line" & Positive'Image (Lexer.Current_Symbol.Line) & ".");
			end case;
			
			Match (D_RIGHT_PARENTHESIS);
			
			Insert     (Tree, Operator, Left);
			Root := Tree;
			Swap_Child (Tree, Tree_1,   Left);
			Swap_Child (Tree, Tree_2,   Right);
		end;
		
		Success := True;
	end if;

	if Success then
		return Root;
	else
		return Tree_1;
	end if;
end Simple_Expression_Support_4;
