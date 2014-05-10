with Ada.Containers.Vectors;
with DXPL.Compiler.Support.Annotation;
with DXPL.Compiler.Support.Parameter;

separate (DXPL.Compiler.Parser)

---------------------------------------------------------
--  script ::=
--    { <with-declaration> [<use-declaration>] }
--    <package-declaration>
--    { <procedure-declaration> | <variables-declaration> | 
--    <function_declaration> } begin { <begin-declaration> } end ";"
--
--  Returns True, if the file could be parsed without
--  errors or False, if an error occurred.
--
--  Exceptions
--    Wrong_Nesting_of_Annotations
--    No_Test_Vectors
--    Configuration_Is_Missing

---------------------------------------------------------
procedure Script (Is_Package_Declaration, Has_Begin : in out Boolean; DXPL_Support : in out DXPL_Support_Type) is
	package Annotation_Manager renames DXPL.Compiler.Support.Annotation;
	package Parameter_Manager  renames DXPL.Compiler.Support.Parameter;

	procedure Function_Declaration is separate;
	--  function_declaration ::=
	--    function <identifier> is | function <identifier> 
	--    "(" <function_parameter_declaration> ")" is

	procedure Variables_Declaration (Item : in out Ada.Strings.Unbounded.Unbounded_String);
	--  forward declaration
	
	procedure Procedure_Parameter_Declaration is
	--  parameter ::=
	--  <identifier> { "," <identifier } ":" [ in | out | in out ] <identifier> [ := <expression> ]
	--
	--  procedure_parameter_declaration ::= 
	--    parameter { ";" parameter }
		
		package Str_Array is new Ada.Containers.Vectors
		  (Element_Type => ASU.Unbounded_String, Index_Type => Positive, "=" => ASU."=");
		Key_Array : Str_Array.Vector;
		--  add all variables to a key vector
		Key, Item : Unbounded_String := Null_Unbounded_String;
		Handover  : Parameter_Manager.Handover := Parameter_Manager.In_Value;
	begin
		Key := Lexer.Current_Symbol.Value;
		--  catch first variable
		Str_Array.Append (Container => Key_Array, New_Item => Key);
 
		--  <identifier> { "," <identifier }
		Match_and_Append (Z_IDENTIFIER, Item);
		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_COLON;
			Match_and_Append (D_COMMA,      Item);
			--  catch up comma separated variables
			Str_Array.Append (Container => Key_Array, New_Item => Lexer.Current_Symbol.Value);
			Match_and_Append (Z_IDENTIFIER, Item);
		end loop;

		--  ":"
		Match_and_Append (D_COLON, Item);

		--  [ in | out | in out ]
		--New_Parameter.Value := Key;
		if Lexer.Current_Symbol.Lex_Element = R_IN then
			Match (R_IN);
		end if;
		if Lexer.Current_Symbol.Lex_Element = R_OUT then
			Match (R_OUT);
			Handover := Parameter_Manager.Out_Value;
		end if;

		declare
			use Str_Array;
			Iterator: Cursor := First (Key_Array);
		begin
			--  add all variables to the parameter list
			while Has_Element (Iterator) loop
				Parameter_Manager.Add_Parameter (Current_Procedure_Name, Element (Iterator), Handover);
				Next (Iterator);
			end loop;
		end;

		--  <identifier>
		if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
			Match_Lenient (Z_IDENTIFIER);
			Match_Lenient (D_DOT);
		end if;
		Match_and_Append (Z_IDENTIFIER, Item);

		--  [ := <expression> ]
		if Lexer.Current_Symbol.Lex_Element = C_ASSIGNMENT then
			Match_and_Append (C_ASSIGNMENT, Item);
			declare
				Tree : Binary_Tree := Expression;
			begin
				Append (Source => Item, New_Item => Syntax_Tree.Get_Infix_Statement (Tree, False, True));
			end;
		end if;

		--  parameter { ";" parameter }
		Append (Source => Item, New_Item => ";");
		if Lexer.Current_Symbol.Lex_Element = D_SEMI_COLON then
			Match (D_SEMI_COLON);
			Procedure_Parameter_Declaration;
		end if;

		--  THE FOLLOWING CODE IS UNTESTED
		--declare
		--	use Str_Array;
		--	Iterator: Cursor := First (Key_Array);
		--begin
		--	while Has_Element (Iterator) loop
		--		Round_State.Add (R_State, Element (Iterator), Item);
		--		Next (Iterator);
		--	end loop;
		--end;

		--  keep track of variables, that are part of the round state
		Round_State.Add (R_State, Key, Item);
	end Procedure_Parameter_Declaration;

	procedure Statements (Annotation_Object : in out Annotation_Manager.Instance) is separate;
	--  statements ::=
	--    <identifier> [ "(" <digit> ")" ] := <expression> ";"

	procedure Procedure_Declaration (DXPL_Support : in out DXPL_Support_Type) is separate;
	--  procedure_declaration ::=
	--    procedure <identifier> is | procedure <identifier> 
	--    "(" <procedure_parameter_declaration> ")" is

	procedure Use_Declaration is separate;
	--  use-declaration ::= 
	--    use <identifier> { "." <identifier> } ";"
	
	procedure Variables_Declaration (Item : in out Ada.Strings.Unbounded.Unbounded_String) is
	--  item ::=
	--    <digit> "=>" <digit>
	--
	--  element ::=
	--    <digit> | <identifier> | "(" ( others "=>" ( <digit> | <identifier> )) | <item> { "," <item> } ")" 
	--
	--  variables_declaration ::=
	--    <identifier> { "," <identifier> } ":" [ constant ] <identifier> [ ":=" <element> ] ";"

	begin
		Debug.Trace ("VARIABLES_DECLARATION with '" & To_String (Lexer.Current_Symbol.Value) & "'");

		Symbol_Table.Push (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_variable);

		--  <identifier> { "," <identifier> }
		if Lexer.Current_Symbol.Lex_Element = Z_ANNOTATION then
			Error (Annotation_Manager.Wrong_Nesting_of_Annotations'Identity,
			  "Annotations are only supported within procedures declaring the hash function.");
		end if;
		Match_and_Append (Z_IDENTIFIER, Item);
		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_COLON;
			Match_and_Append (D_COMMA, Item);
			Symbol_Table.Push (S_Table, Lexer.Current_Symbol.Value, Symbol_Table.t_variable);
			Match_and_Append (Z_IDENTIFIER, Item);
		end loop;

		--  ":" [ constant ]
		Match_and_Append (D_COLON, Item);
		-- TODO (hoppe): remove aliased!
		Append (Source => Item, New_Item => "aliased ");
		if Lexer.Current_Symbol.Lex_Element = R_CONSTANT then
			Match_and_Append (R_CONSTANT, Item);
		end if;

		loop  --  discard
			exit when Lexer.Current_Symbol.Lex_Element = D_SEMI_COLON;
			if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
				Match_Lenient (Z_IDENTIFIER);
				Match_Lenient (D_DOT);
			end if;
			if Lexer.Current_Symbol.Lex_Type = TYPE_DELIMITER then
				Append (Source => Item, New_Item => Lexer.Current_Symbol.Value);
			else
				Append (Source => Item, New_Item => Lexer.Current_Symbol.Value & " ");
			end if;
			Next_Symbol;
		end loop;
		Match_and_Append (D_SEMI_COLON, Item);
	end Variables_Declaration;
	
	procedure With_Declaration is separate;
	--  with_declaration ::= 
	--    with <identifier> { "." <identifier> } ";"

	package Simple_Vector is new Ada.Containers.Vectors
		(Element_Type => Boolean, Index_Type => Positive);

	type Test_Vector_Type is
		record
			Item  : ASU.Unbounded_String := ASU.Null_Unbounded_String;
			Count : Positive             := Positive'First;
			Keys  : Simple_Vector.Vector;
		end record;

	procedure Read_Line is
	begin
		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_SEMI_COLON;
			Trace;
			Next_Symbol;
		end loop;
	end Read_Line;

	procedure Begin_Declaration (Test_Vector       : in out Test_Vector_Type;
								 DXPL_Support      : in DXPL_Support_Type;
								 Has_Configuration : in out Boolean;
								 Has_Test_Vector   : in out Boolean) is separate;
	--  begin_declaration ::=
	--    begin ( <configuration>, <test>, { test_vector } )
	--    end [ <identifier> ] ";"

	Former_File : Ada.Text_IO.File_Type := Ada.Text_IO.Current_Output;
begin
	Ada.Text_IO.Set_Output (File => File_Round);
		
	case Lexer.Current_Symbol.Lex_Element is
		--  <with-declaration>
		when R_WITH =>
			With_Declaration;
		
		--  [ <use-declaration> ] 
		when R_USE =>
			Use_Declaration;
			
		--  <function_declaration>
		when R_FUNCTION =>
			Function_Declaration;
			
		when R_PROCEDURE =>
			--  package_declaration
			--    ::= package body <identifier> is
			if Is_Package_Declaration then
				Trace ("package body ");
				Match (R_PROCEDURE);
				Trace ("DXPL.Round_Function ");
				Match (Z_IDENTIFIER);
				Match_and_Trace (R_IS);
				
				Is_Package_Declaration := False;
			
			--  <procedure-declaration>
			else
				Procedure_Declaration (DXPL_Support);
			end if;

		--  <begin-declaration>
		when R_BEGIN =>
			Has_Begin := True;
			declare
				Test_Vector : Test_Vector_Type;
				Has_Configuration, Has_Test_Vector : Boolean := False;
			begin
				Begin_Declaration (Test_Vector, DXPL_Support, Has_Configuration, Has_Test_Vector);
				if not Has_Configuration then
					Error (Configuration_Is_Missing'Identity,
					  "DXPL.Configuration is missing. Please revise your description.");
				end if;
				if not Has_Test_Vector then
					Error (No_Test_Vectors'Identity,
					  "You've not added any test vectors. Your description cannot be verified.");
				end if;
			end;

			if not DXPL_Support.Has_Process then
				Error (DXPL_Procedure_Exception'Identity,
				  "No 'DXPL_Process' procedure was found. Please revise your code.");
			end if;

		when R_END =>
			Match_and_Trace (R_END);
			Match (Z_IDENTIFIER);
			Match_and_Trace (D_SEMI_COLON);

		--  <variables-declaration>
		when Z_IDENTIFIER =>
			declare
				New_Line : Unbounded_String := Null_Unbounded_String;
			begin
				Variables_Declaration (New_Line);
				Trace (To_String (New_Line));
			end;
			
		--  subtype .. ";"
		when R_SUBTYPE =>
			Read_Line;
			Match_and_Trace (D_SEMI_COLON);
		
		--  type .. ","
		when R_TYPE =>
			Read_Line;
			Match_and_Trace (D_SEMI_COLON);

		when Z_ANNOTATION =>
			Error (Annotation_Manager.Wrong_Nesting_of_Annotations'Identity,
			  "Annotations are only supported within procedures declaring the hash function.");

		--  handle all other constructs, 
		--  which are currently not relevant for us
		when others =>
			Trace;
			Next_Symbol;
	end case;
	
	--  Recursive call
	if Has_More_Symbols then
		Ada.Text_IO.Set_Output (File => Former_File);
		Script (Is_Package_Declaration, Has_Begin, DXPL_Support);
	elsif not Has_Begin then
		Error (Begin_Is_Missing'Identity,
		  "The body of this procedure is missing. Please declare a suitable " &
		  "'begin' block including DXPL.Configuration and at least one " &
		  "test vector.");
	end if;
end Script;
