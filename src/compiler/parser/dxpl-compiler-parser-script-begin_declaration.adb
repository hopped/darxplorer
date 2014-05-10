with Ada.Containers.Vectors;

separate (DXPL.Compiler.Parser.Script)

---------------------------------------------------------
--  begin_declaration ::=
--    begin ( <configuration>, <test>, { test_vector } )
--    end [ <identifier> ] ";"
--
--  Exceptions
--    None
--
--  TODO:
--    Raise an error, if more than one instance of
--    DXPL.Configuration or DXPL.Test is declared.
---------------------------------------------------------
procedure Begin_Declaration (Test_Vector       : in out Test_Vector_Type;
							 DXPL_Support      : in DXPL_Support_Type;
							 Has_Configuration : in out Boolean;
							 Has_Test_Vector   : in out Boolean) is

	procedure Add_Init_and_Finalize_Statements (DXPL_Support : in DXPL_Support_Type) is
	--  Adds separate functions to allow initialization and finalization,
	--  if no correspondend message is given in the description of the
	--  hash function. This means, that the variables
	--  'DXPL_Support.Has_Initialize' resp. 'DXPL_Support.Has_Finalize'
	--  are set to False.
	begin
		If not DXPL_Support.Has_Initialize then
			Trace ("procedure Initialize (State : access Round_State) is separate; ");
		end if;

		if not DXPL_Support.Has_Finalize then
			Trace ("procedure Finalize (State : access Round_State) is separate; ");
		end if;

		Trace ("function Number_of_Tests return Positive is separate; ");
	end Add_Init_and_Finalize_Statements;

	procedure Finalize_Test_Vector (Test_Vector : in out Test_Vector_Type) is
	begin
		declare
		--  check, if test vectors are inconsistent regarding
		--  declaration of keys
			use Simple_Vector;
			Iterator : Cursor := First (Test_Vector.Keys);
			Count_True, Count_False : Natural := Natural'First;
		begin
			while Has_Element (Iterator) loop
				declare
					Value : Boolean := Element (Iterator);
				begin
					if Value then
						Count_True := Natural'Succ (Count_True);
					else
						Count_False := Natural'Succ (Count_False);
					end if;
				end;
				Next(Iterator);
			end loop;

			if Count_True > 0 and Count_False > 0 then
				Error (Inconsistent_Vectors'Identity,
				  "Declaration of test vectors is inconsistent. Please revise your code.");
			end if;
		end;

		Append (Source => Test_Vector.Item, New_Item => "); ");
		Trace  (To_String (Test_Vector.Item));

		Ada.Text_IO.Set_Output (File => File_Tests);
		declare
			Number : Natural := Count (Test_Vector.Item, "Message");
		begin
			Trace ("separate (DXPL.Round_Function) ");
			Trace ("function Number_of_Tests return Positive is ");
			Trace ("begin ");
			Trace ("return " & Positive'Image (Number) & ";" );
			Trace ("pragma Inline (Number_of_Tests); ");
			Trace ("end Number_of_Tests; ");
		end;
		Ada.Text_IO.Set_Output (File => File_Round);
	end Finalize_Test_Vector;

	procedure Handle_Array (Test_Vector : in out Test_Vector_Type; Counter : in out Positive) is
	--  element ::=
	--  { <digit> } => { <digit> }
	--
	--  handle_array ::= 
	--  "(" element { "," element } ")" ";"
	begin
		Counter := Positive'First;

		Match_and_Append (L_NUMERIC, Test_Vector.Item);
		if Lexer.Current_Symbol.Lex_Element = C_NAMED then
			Match_and_Append (C_NAMED,   Test_Vector.Item);
			Match_and_Append (L_NUMERIC, Test_Vector.Item);
		end if;

		loop
			exit when Lexer.Current_Symbol.Lex_Element = D_RIGHT_PARENTHESIS;
				Match_and_Append (D_COMMA,   Test_Vector.Item);
				Match_and_Append (L_NUMERIC, Test_Vector.Item);
				if Lexer.Current_Symbol.Lex_Element = C_NAMED then
					Match_and_Append (C_NAMED,   Test_Vector.Item);
					Match_and_Append (L_NUMERIC, Test_Vector.Item);
				end if;
				Counter := Positive'Succ (Counter);
		end loop;
	end Handle_Array;
	
	procedure Handle_Configuration is
	--  handle_configuration ::= 
	--    [ <identifier> "." ] <identifier> := "(" 
	--    [ Algorithm => ] <identifier> "." <identifier> "," 
	--    [ Rounds    => ] { <digit> } ")" ","
	--    [ [ Key_Size => ] { <digit> } ")" "," ]
	--    [ DXPL_TERMINATION => ] { <digit> } ")" ";"
	begin
		Match (Z_IDENTIFIER);
		Match (D_LEFT_PARENTHESIS);

		-- Algorithm
		Match_Lenient (Z_IDENTIFIER);
		Match_Lenient (C_NAMED);
		GLOBAL_Configuration.Algorithm := Unbounded_Slice (Lexer.Current_Symbol.Value, 2, Length (Lexer.Current_Symbol.Value) - 1);
		Match (L_STRING);
		Match (D_COMMA);

		-- Rounds
		Match_Lenient (Z_IDENTIFIER);
		Match_Lenient (C_NAMED);
		GLOBAL_Configuration.Rounds := Positive'Value (To_String (Lexer.Current_Symbol.Value));
		Match (L_NUMERIC);
		Match (D_COMMA);

		--  Key_Size
		if Lexer.Current_Symbol.Lex_Element = D_COMMA then
			Match (D_COMMA);
			Match_Lenient (Z_IDENTIFIER);
			Match_Lenient (C_NAMED);
			GLOBAL_Configuration.Keysize := Positive'Value (To_String (Lexer.Current_Symbol.Value));
			Match (L_NUMERIC);
			Match (D_COMMA);
		end if;

		--  Termination value
		Match_Lenient (Z_IDENTIFIER);
		Match_Lenient (C_NAMED);
		GOBAL_Test.Probability_for_Termination := Positive'Value (To_String (Lexer.Current_Symbol.Value));
		Match (L_NUMERIC);

		-- ")" ";"
		Match (D_RIGHT_PARENTHESIS);
		Match (D_SEMI_COLON);
		
		--  Writes rounds to file
		Ada.Text_IO.Set_Output (File => File_DXPL);
		Trace ("subtype Rounds is Positive range 1 .. ");
		Trace (Positive'Image (GLOBAL_Configuration.Rounds) & "; ");
		Trace ("end DXPL; ");
		Ada.Text_IO.Set_Output (File => File_Round);
	end Handle_Configuration;

	procedure Handle_Module_Sequence is
	--  element ::=
	--    <digit> => "Process_" <digit> "'Access"
	--
	--  handle_module_sequence ::=
	--    Module_Sequence := "(" <element> { "," <element> } ")" ";"
	begin
		Trace ("Module_Sequence := ( 1 => Process_1'Access");
		for I in 2 .. Process_Procedure.Get_Count - 1 loop
			declare
				Str_Count : String := Ada.Strings.Fixed.Delete (Integer'Image (I), 1, 1);
			begin
				Trace ("," & Integer'Image (I) & "=> Process_" & Str_Count & "'Access");
			end;
		end loop;
		Trace (");");
	end Handle_Module_Sequence;
	
--	procedure Handle_Test is
--	--  handle_test ::= 
--	--    [ <identifier> "." ] <identifier> := "(" 
--	--    [ <identifier> =>  ] { <digit> }  ","
--	--    [ <identifier> =>  ] <identifier> "." <identifier> "," 
--	--    [ <identifier> =>  ] { <digit> }  "," 
--	--    [ <identifier> =>  ] <identifier> "." <identifier> ")" ";"
--	begin
--		Match (Z_IDENTIFIER);
--		Match (D_LEFT_PARENTHESIS);
--		
--		--  start analyse in round
--		Match_Lenient (Z_IDENTIFIER);
--		Match_Lenient (C_NAMED);
--		GOBAL_Test.Round_Offset := Positive'Value (To_String (Lexer.Current_Symbol.Value));
--		Match (L_NUMERIC);
--		Match (D_COMMA);
--
--		--  hamming weight
--		Match_Lenient (Z_IDENTIFIER);
--		Match_Lenient (C_NAMED);
--		Match (Z_IDENTIFIER);
--		Match (D_DOT);
--		declare
--			Weight : Unbounded_String := Delete (Lexer.Current_Symbol.Value, 2, 3);
--		begin
--			if    Weight = "1" then
--				GOBAL_Test.Hamming_W := HW_1;
--			elsif Weight = "2" then
--				GOBAL_Test.Hamming_W := HW_2;
--			elsif Weight = "3" then
--				GOBAL_Test.Hamming_W := HW_3;
--			else -- Weight = 4
--				GOBAL_Test.Hamming_W := HW_4;
--			end if;
--		end;
--		Match (Z_IDENTIFIER);
--		Match (D_COMMA);
--
--		--  termination value
--		Match_Lenient (Z_IDENTIFIER);
--		Match_Lenient (C_NAMED);
--		GOBAL_Test.Probability_for_Termination := Positive'Value (To_String (Lexer.Current_Symbol.Value));
--		Match (L_NUMERIC);
--		Match (D_COMMA);
--		
--		--  recognition of technique
--		Match_Lenient (Z_IDENTIFIER);
--		Match_Lenient (C_NAMED);
--		Match (Z_IDENTIFIER);
--		Match (D_DOT);
--		declare
--			Initial : Character := Element (Lexer.Current_Symbol.Value, 1);
--		begin
--			case Initial is
--			when 'L' => 
--				GOBAL_Test.Chosen_Technique := Lazy_Laura;
--			when 'P' => 
--				GOBAL_Test.Chosen_Technique := Pedantic_Petra;
--			when 'G' =>
--				GOBAL_Test.Chosen_Technique := Greedy_Grete;
--			when 'T' =>
--				GOBAL_Test.Chosen_Technique := Trusty_Trudy;
--			when others =>
--				Error (Wrong_Technique'Identity, "You've encountered a wrong technique: " & To_String (Lexer.Current_Symbol.Value));
--			end case;
--		end;
--		Match (Z_IDENTIFIER);
--
--		--  end
--		Match (D_RIGHT_PARENTHESIS);
--		Match (D_SEMI_COLON);
--	end Handle_Test;
	
	procedure Handle_Testvectors (Test_Vector : in out Test_Vector_Type) is
	--  handle_testvectors ::= 
	--    [ <identifier> "." ] <identifier> := "("
	--    [ Message =>  ] <handle_array> ","
	--    [ [ Key   =>  ] <handle_array> "," ]
	--    [ Digest  =>  ] <handle_array> ")" ";"
		Message_Size : Positive := Positive'First;
		Digest_Size  : Positive := Positive'First;
	begin
		if Test_Vector.Count = Positive'First then
			Append (Source   => Test_Vector.Item,
					New_Item => "Test_Vectors.Batch := new Validation_Array'( 1 => ");
		else
			Append (Source   => Test_Vector.Item, New_Item => ",");
			Append (Source   => Test_Vector.Item,
			 		New_Item => Positive'Image (Test_Vector.Count) & " => ");
		end if;
		Match (Z_IDENTIFIER);
		Match (D_LEFT_PARENTHESIS);

		Test_Vector.Count := Positive'Succ (Test_Vector.Count);

		--  Message => <array> ","
		Match_Lenient (Z_IDENTIFIER);
		Match_Lenient (C_NAMED);
		Match_and_Append (D_LEFT_PARENTHESIS, Test_Vector.Item);
		Append (Source => Test_Vector.Item, New_Item => "Message => new Word_Array'( ");

		Handle_Array (Test_Vector, Message_Size);
		Debug.Trace ("Message Size :" & Positive'Image (Message_Size));

		Match_and_Append (D_RIGHT_PARENTHESIS, Test_Vector.Item);
		Match_and_Append (D_COMMA, Test_Vector.Item);

		--  Key => <array> ","
		if Lexer.Current_Symbol.Value = "DXPL_KEY" then
			Simple_Vector.Append (Test_Vector.Keys, True);

			Round_State.Set_Key_Support (R_State, True);

			Match_Lenient (Z_IDENTIFIER);
			Match_Lenient (C_NAMED);
			Match (D_LEFT_PARENTHESIS);
			Append (Source => Test_Vector.Item, New_Item => "Key => new Word_Array'( ");

			declare
				Counter : Positive := Positive'First;
			begin
				Handle_Array (Test_Vector, Counter);
				Debug.Trace ("Key Size :" & Positive'Image (Counter));
			end;

			Match_and_Append (D_RIGHT_PARENTHESIS, Test_Vector.Item);
			Match_and_Append (D_COMMA, Test_Vector.Item);

		--  insert dummy values to maintain compatibility
		else
			Simple_Vector.Append (Test_Vector.Keys, False);

			Append (Source => Test_Vector.Item, New_Item => "Key => new Word_Array'( ");
			Append (Source => Test_Vector.Item, New_Item => "1 => 16#0000000000000000#, ");
			Append (Source => Test_Vector.Item, New_Item => "2 => 16#0000000000000000#, ");
			Append (Source => Test_Vector.Item, New_Item => "3 => 16#0000000000000000#, ");
			Append (Source => Test_Vector.Item, New_Item => "4 => 16#0000000000000000#), ");

		end if;

		--  Digest => <array> ")"
		Match_Lenient (Z_IDENTIFIER);
		Match_Lenient (C_NAMED);
		Match (D_LEFT_PARENTHESIS);
		Append (Source => Test_Vector.Item, New_Item => "Digest => new Word_Array'( ");

		Handle_Array (Test_Vector, Digest_Size);
		Debug.Trace ("Digest Size :" & Positive'Image (Digest_Size));
		
		if Message_Size /= Digest_Size then
			declare
				Digest_Str : String := Ada.Strings.Fixed.Trim (Positive'Image (Digest_Size), Ada.Strings.Left);
			begin
				Round_State.Add
				  (R_State,
				   To_Unbounded_String ("DXPL_Digest"),
				   To_Unbounded_String ("DXPL_Digest : Word_Array_" & Digest_Str & " := (others => 16#0#);"));
			end;
		end if;

		Match_and_Append (D_RIGHT_PARENTHESIS, Test_Vector.Item);

		--  end
		Match_and_Append (D_RIGHT_PARENTHESIS, Test_Vector.Item);
		Match (D_SEMI_COLON);
	end Handle_Testvectors;

begin
	case Lexer.Current_Symbol.Lex_Element is
		--  begin
		when R_BEGIN =>
			--  Process tasks, which should be
			--  finished before we proceed with 'begin'

			--  a) Add separate functions to allow validation support
			Trace ("function Get_Message (Index : in Positive) return Word_Array is separate; ");
			Trace ("function Get_Digest  (Index : in Positive) return Word_Array is separate; ");
			Trace ("function Get_Key     (Index : in Positive) return Word_Array is separate; ");

			--  b) Add separate functions to allow initialization and finalization
			Add_Init_and_Finalize_Statements (DXPL_Support);

			--  begin
			Match_and_Trace (R_BEGIN);

			--  c) Serialize the variable 'Module_Sequence'
			Handle_Module_Sequence;

			--  d) Recursive call
			Begin_Declaration (Test_Vector, DXPL_Support, Has_Configuration, Has_Test_Vector);

		--  ( <configuration>, <test>, { test_vector } ) 
		when Z_IDENTIFIER =>
			if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
				Match (Z_IDENTIFIER);
				Match (D_DOT);
			end if;

			case Lexer.Current_Symbol.Lex_Type is
				--  <configuration>
				when TYPE_DARX_CONFIGURATION =>
					if not Has_Configuration then
						Handle_Configuration;
						Has_Configuration := True;
					else
						Error (Configuration_Error'Identity,
						  "More than one DXPL.Configuration is given - only one is allowed.");
					end if;

				--  { test_vector }
				when TYPE_DARX_TESTVECTOR =>
					Handle_Testvectors (Test_Vector);
					Has_Test_Vector := True;

				--  <test>
				--when TYPE_DARX_TEST =>
				--	Handle_Test;

				--  An error has occurred
				when others =>
					Error (Unsupported_Declaration'Identity, "This declaration is not supported: " & 
						Lexical_Element'Image(Lexer.Current_Symbol.Lex_Element) & " " & 
						To_String (Lexer.Current_Symbol.Value));
			end case;
		
			--  Recursive call
			Begin_Declaration (Test_Vector, DXPL_Support, Has_Configuration, Has_Test_Vector);

		--  end [ <identifier> ] ";"
		when R_END =>
			--  Process tasks, that has to be executed
			--  before we finish the parsing job.

			--  Writes test vectors to the file
			Finalize_Test_Vector (Test_Vector);

			--  Writes state information to a file
			Round_State.Trace
			  (R_State, Support.Directory,
			   Support.Filename_Round_State,
			   ASU.To_String (GLOBAL_Configuration.Algorithm));

			--  End of file reachead
			return;

		when Z_ANNOTATION =>
			Error (Annotation_Manager.Wrong_Nesting_of_Annotations'Identity,
			  "The given annotation in line" &
			  Positive'Image (Lexer.Current_Symbol.Line) & 
			  " is not supported. Annotations " &
			  "are only allowed in procedures declaring the hash function.");

		--  An error has occured
		when others =>
			Error (Wrong_Element'Identity);
	end case;
end Begin_Declaration;
