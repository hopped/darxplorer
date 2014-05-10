with Ada.Strings.Unbounded;
with Ada.Text_IO;
with DXPL.Compiler.Lexer;

pragma Elaborate_All (DXPL.Compiler.Lexer);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 06/07/2009
--!  Last-Modified :: 08/06/2009

--!  Purpose:
--!  Parses a given description of a hash function. The hash
--!  function is defined in a simplified Ada syntax. This allows
--!  a suitable parser to transform the given description in
--!  in source code, which can be integrated into the darXplorer
--!  framework for further analyzation.
----------------------------------------------------------------
generic
	Iteration_Threshold : Positive := 80;
	--  Used to limit the maximum number of iterations for 'for' loops,
	--  because our compiler will proceed with loop unrolling.

package DXPL.Compiler.Parser is
	function Parse (Filename : in String) return Boolean;
	--  Loads a given file by means of a concrete lexer
	--  and starts the parsing process. After parsing,
	--  all file handlers are closed properly.
	--
	--  Returns True, if the file could be parsed without
	--  errors or False, if an error occurred.
	--
	--  TODO:
	--    Error detection circumvents a return value of
	--    false, because an error is raised before.

private
	package ASU renames Ada.Strings.Unbounded;

	procedure Lexer_Error (Type_of_Exception : in Ada.Exceptions.Exception_Id; Message : in String);
	package Lexer is new DXPL.Compiler.Lexer (Lexer_Error);
	
	Current_State  : Lexer.Lexer_State;
	--  Keeps the state of the lexer
	
	File_Round : Ada.Text_IO.File_Type;
	File_DXPL  : Ada.Text_IO.File_Type;
	File_Tests : Ada.Text_IO.File_Type;
	File_Darxplorer : Ada.Text_IO.File_Type;

	use Ada.Strings.Unbounded;

	type Configuration_Type is
		--  Used to gather information about settings, a user
		--  defined in the description of a hash function. Settings
		--  comprise the name of the algorithm, the number of
		--  max. rounds and the keysize.
		record
			Algorithm  : ASU.Unbounded_String := ASU.Null_Unbounded_String;
			Rounds     : Positive;
			Keysize    : Positive;
		end record;
	GLOBAL_Configuration : Configuration_Type;
		
	-- need to re-define some types, because we cannot include DXPL.Types
	type Hamming_Weight is (HW_1, HW_2, HW_3, HW_4);
	type Technique      is (Lazy_Laura, Greedy_Grete, Pedantic_Petra, Trusty_Trudy, Fuzzy_Fiona);
	
	type Test_Type is
		--  Test encapsulates information about a desired test,
		--  that should be executed for the hash function. The
		--  round offset declares, in which round the analyzation
		--  should start.
		record
			--Round_Offset 				: Positive 		 := 1;
			--Hamming_W    				: Hamming_Weight := HW_1;
			Probability_for_Termination : Positive		 := 256;
			--Chosen_Technique			: Technique 	 := Trusty_Trudy;
		end record;
	GOBAL_Test : Test_Type;

	Current_Procedure_Name : Unbounded_String := Null_Unbounded_String;
end DXPL.Compiler.Parser;
