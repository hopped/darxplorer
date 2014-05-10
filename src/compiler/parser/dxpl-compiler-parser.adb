with Ada.Exceptions;			use Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with DXPL.Compiler.Debug;
with DXPL.Compiler.Vocabulary;	use DXPL.Compiler.Vocabulary;
with DXPL.Compiler.Support;
with DXPL.Compiler.Syntax_Tree;
with DXPL.Compiler.Support.Input_Manager;
with DXPL.Compiler.Support.Process_Procedure;
with DXPL.Compiler.Support.Annotation;
with DXPL.Compiler.Support.State;
with DXPL.Compiler.Symbol_Table;

pragma Elaborate_All (DXPL.Compiler.Syntax_Tree);
pragma Elaborate_All (DXPL.Compiler.Support.Input_Manager);

package body DXPL.Compiler.Parser is
	package Support           renames DXPL.Compiler.Support;
	package Process_Procedure renames DXPL.Compiler.Support.Process_Procedure;
	package Input_Manager     is new  DXPL.Compiler.Support.Input_Manager (Lexer);
	package Round_State       renames DXPL.Compiler.Support.State;
	package Symbol_Table      renames DXPL.Compiler.Symbol_Table;
	package Debug             renames DXPL.Compiler.Debug;

	S_Table : Symbol_Table.Instance;
	R_State : Round_State.Instance;
	package Syntax_Tree is new DXPL.Compiler.Syntax_Tree (S_Table, R_State);
	use Syntax_Tree;
	use Binary_Trees;

	function Has_More_Symbols return Boolean is
	begin
		case Input_Manager.Get_Input is
			when FILE =>
				return Lexer.Has_More_Symbols (Current_State);

			when MEMORY =>
				return Input_Manager.Has_More_Symbols;
		end case;
	end Has_More_Symbols;

	procedure Next_Symbol is
	begin
		case Input_Manager.Get_Input is
			when FILE =>
				if Lexer.Has_More_Symbols (Current_State) then
					Lexer.Next_Symbol (Current_State, Lexer.Current_Symbol);
				end if;

			when MEMORY =>
				if Input_Manager.Has_More_Symbols then
					Input_Manager.Next_Symbol (Lexer.Current_Symbol);
				end if;
		end case;
	end Next_Symbol;

	procedure Trace (Item : in String) is
	begin
		Ada.Text_IO.Put (Item);
	end Trace;

	procedure Trace (Item : in ASU.Unbounded_String) is
	begin
		Ada.Text_IO.Put (To_String (Item));
	end Trace;

	procedure Trace (Item : in Lexer.Symbol_Type) is
	begin
		if Item.Lex_Type /= TYPE_DELIMITER then
			ASU.Append (Source => Lexer.Current_Symbol.Value, New_Item => " ");
		end if;
		Trace (Lexer.Current_Symbol.Value);
	end Trace;

	procedure Trace is
	begin
		Trace (Lexer.Current_Symbol);
	end Trace;
	
	procedure Close is
	begin
		-- clear some data structures
		Input_Manager.Clear;
		Process_Procedure.Clear;
		
		Ada.Text_IO.Flush;

		--  close all files
		Ada.Text_IO.Close (File => File_Round);
		Ada.Text_IO.Close (File => File_DXPL);
		Ada.Text_IO.Close (File => File_Tests);
		Ada.Text_IO.Close (File => File_DARXPLORER);

		--  status error, if we do not reset the output stream
		--  due to following outputs by means of 'put' or 'put_line'
		Ada.Text_IO.Set_Output(File => Ada.Text_IO.Standard_Output);
	end;

	procedure Error (Item : Exception_Id; Message : in String := "") is
	begin
		Close;
		Raise_Exception (Item, Message);
	end Error;

	procedure Lexer_Error (Type_of_Exception : in Ada.Exceptions.Exception_Id; Message : in String) is
	begin
		Error (Type_of_Exception, Message);
	end Lexer_Error;

	procedure Match (Expected_Element : in Lexical_Element) is
	begin
		if Lexer.Current_Symbol.Lex_Element = Expected_Element then
			Next_Symbol;
		else
			Error (Wrong_Element'Identity, "Match :: You've encountered a wrong element: " & 
				   Lexical_Element'Image(Lexer.Current_Symbol.Lex_Element) & " with value " &
				   ASU.To_String (Lexer.Current_Symbol.Value) &
				   ". We expected : " & Lexical_Element'Image(Expected_Element) &
				   " in line" & Positive'Image (Lexer.Current_Symbol.Line) &
				   ", column" & Positive'Image (Lexer.Current_Symbol.Column));
		end if;
	end Match;

	procedure Match_Lenient (Expected_Element : in Lexical_Element) is
	begin
		if Lexer.Current_Symbol.Lex_Element = Expected_Element then
			Next_Symbol;
		end if;
	end Match_Lenient;

	procedure Match_and_Trace (Expected_Element : in Lexical_Element) is
	begin
		if Lexer.Current_Symbol.Lex_Element = Expected_Element then
			Trace;
			Next_Symbol;
		else
			Error (Wrong_Element'Identity, "Match_and_Trace :: You've encountered a wrong element: " & 
				   Lexical_Element'Image(Lexer.Current_Symbol.Lex_Element) & " with value " &
				   ASU.To_String (Lexer.Current_Symbol.Value) &
				   ". We expected : " & Lexical_Element'Image(Expected_Element) &
				   " in line" & Positive'Image (Lexer.Current_Symbol.Line) &
				   ", column" & Positive'Image (Lexer.Current_Symbol.Column));
		end if;
	end;

	procedure Match_and_Append (Expected_Element : in Lexical_Element; Source : in out ASU.Unbounded_String) is
	begin
		if Lexer.Current_Symbol.Lex_Element = Expected_Element then
			if Lexer.Current_Symbol.Lex_Element = D_DOT then
				ASU.Append (Source => Source, New_Item => Lexer.Current_Symbol.Value);
			else
				ASU.Append (Source => Source, New_Item => Lexer.Current_Symbol.Value & " ");
			end if;
			Next_Symbol;
		else
			Error (Wrong_Element'Identity, "Match_and_Append :: You've encountered a wrong element: " & 
				   To_String (Lexer.Current_Symbol.Value) & " (" & Lexical_Element'Image(Lexer.Current_Symbol.Lex_Element) &
				   "). We expected : " & Lexical_Element'Image(Expected_Element) &
				   " in line" & Positive'Image (Lexer.Current_Symbol.Line) &
				   ", column" & Positive'Image (Lexer.Current_Symbol.Column));
		end if;
	end Match_and_Append;

	function Expression return Binary_Trees.Binary_Tree;
	-- forward declaration

	generic
		with function Parse_Expression return Binary_Tree;
		with function Is_Operator return Boolean;
	function Simple_Expression_Support return Binary_Trees.Binary_Tree;
	
	function Primary return Binary_trees.binary_tree;
	
	function Simple_Expression_Support_4 return Binary_Tree is separate;

	function Simple_Expression_Support return Binary_Tree is separate;
	--  primary ::=
	--    "(" <expression> ")" | <identifier> [ "(" <expression> { "," <expression> } ")" ]
	--
	--  <Parse_Expression> ::= 
	--    <primary> [ "**" <primary> ] [ "*" | mod | "/" ] [ "+" | "-" ]

	function Is_Mod return Boolean is
	begin
		return Lexer.Current_Symbol.Lex_Element = R_MOD;
	end Is_Mod;

	function Simple_Expression_Support_3 is new Simple_Expression_Support
		(Parse_Expression => Simple_Expression_Support_4,
		 Is_Operator      => Is_Mod);

	function Is_Multiplication_or_Division return Boolean is
	begin
		return Lexer.Current_Symbol.Lex_Element = D_ASTERISK or Lexer.Current_Symbol.Lex_Element = D_SOLIDUS;
	end Is_Multiplication_or_Division;

	function Simple_Expression_Support_2 is new Simple_Expression_Support
		(Parse_Expression => Simple_Expression_Support_3,
		 Is_Operator      => Is_Multiplication_or_Division);

	function Is_Plus_or_Minus return Boolean is
	begin
		return Lexer.Current_Symbol.Lex_Element = D_PLUS or Lexer.Current_Symbol.Lex_Element = D_MINUS;
	end Is_Plus_or_Minus;

	function Simple_Expression_Support_1 is new Simple_Expression_Support
		(Parse_Expression => Simple_Expression_Support_2,
		 Is_Operator      => Is_Plus_or_Minus);

	function Is_And_or_XOR_or_OR return Boolean is
	begin
		return Lexer.Current_Symbol.Lex_Element = R_AND or Lexer.Current_Symbol.Lex_Element = R_OR or Lexer.Current_Symbol.Lex_Element = R_XOR;
	end Is_And_or_XOR_or_OR;

	function Simple_Expression_Support_0 is new Simple_Expression_Support
		(Parse_Expression => Simple_Expression_Support_1,
		 Is_Operator      => Is_And_or_XOR_or_OR);

	function Primary return Binary_Tree is separate;
	--  primary ::=
	--  "(" <expression> ")" | <identifier> [ "(" <expression> { "," <expression> } ")" ]

	function Simple_Expression return Binary_Tree is separate;
	--  primary ::=
	--  "(" <expression> ")" | <identifier> [ "(" <expression> { "," <expression> } ")" ]
	--
	--  simple_expression_support ::= 
	--  <primary> [ "**" <primary> ] [ "*" | mod | "/" ] [ "+" | "-" ]
	--
	--  simple_expression ::=
	--  [ "+" | "-" | "not" ] <simple_expression_support> { <simple_expression_support> }

	function Expression return Binary_Tree is separate;
	--  expression ::=
	--  <identifier> [ "(" <digit> ")" ] "+" | "-" | xor | or | and | mod <identifier> [ "(" <digit> ")" ] ";" |
	--  <identifier> "(" <variable> "," <digit> ")" ";"
	
	--  element ::=
	--  <simNext_Symbolple_expression> [ [ "=" | "/=" | "<" | "<=" | ">" | ">=" ] <simple_expression> ] [ [ and | or | xor ] <simple_expression> ]
	--
	--  expression ::= <element> { <element> }

	type DXPL_Support_Type is
		record
			Has_Process    : Boolean := False;
			Has_Initialize : Boolean := False;
			Has_Finalize   : Boolean := False;
		end record;

	procedure Script (Is_Package_Declaration, Has_Begin : in out Boolean; DXPL_Support : in out DXPL_Support_Type) is separate;
	--  script ::=
	--    { <with-declaration> [<use-declaration>] }
	--    <package-declaration>
	--    { <procedure-declaration> | <variables-declaration> }
	--    begin { <begin-declaration> } end ";"

	function Parse (Filename : in String) return Boolean is

		procedure Create_Tests;
		--  A simple Ada file is produced to validate the
		--  given description of a hash function.
		--
		--  Preconditions
		--   - Two file descriptors, named 'File_Darxplorer' and
		--     'File_Round' has to be initalized in advance.
		--   - Probability threshold for termination of the 
		--     analyzation process has to be read into
		--     'Test.Probability_for_Termination'
		--   - The name of the algorithm has to be read
		--     into the variable 'Configuration.Algorithm'

		procedure Create_Tests is
			Former_File : Ada.Text_IO.File_Type := Ada.Text_IO.Current_Output;
		begin
			Ada.Text_IO.Set_Output (File => File_Darxplorer);

			Trace ("with DXPL.Types; use DXPL.Types; ");
			Trace ("procedure DXPL.DARXPLORER is Threshold : Conditioned_Float := 2.0 ** (- (Integer ( ");
			Trace (Positive'Image (GOBAL_Test.Probability_for_Termination) & "))); ");
			Trace ("Number_of_Modules : Integer := " & Positive'Image (Process_Procedure.Get_Count - 1) & "; ");
			Trace ("Name : String := """ & To_String (GLOBAL_Configuration.Algorithm) & """; ");
			Trace ("procedure Process is separate; ");
			Trace ("begin Process; end DXPL.DARXPLORER;");

			Ada.Text_IO.Set_Output (File => Former_File);
		end Create_Tests;

		procedure Open_Files is
			package IO renames Ada.Text_IO;
		begin
			IO.Create (File => File_Round,
					   Mode => IO.Out_File,
					   Name => Support.Directory & "dxpl-round_function.adb");
			IO.Create (File => File_DXPL,
					   Mode => IO.Out_File,
					   Name => Support.Directory & "dxpl.ads");
			IO.Create (File => File_Tests,
					   Mode => IO.Out_File,
					   Name => Support.Directory & "dxpl-round_function-number_of_tests.adb");
			IO.Create (File => File_Darxplorer,
					   Mode => IO.Out_File,
					   Name => Support.Directory & "dxpl-darxplorer.adb");
		end;

	begin
		Open_Files;

		declare
			Is_Package_Declaration : Boolean := True;
			Has_Begin    : Boolean := False;
			DXPL_Support : DXPL_Support_Type;
		begin
			Current_State := Lexer.Init (Filename);
			-- set to first symbol
			Next_Symbol;
			Script (Is_Package_Declaration, Has_Begin, DXPL_Support);
			Create_Tests;

			Close;

			return True;
		end;
	end Parse;
end DXPL.Compiler.Parser;
