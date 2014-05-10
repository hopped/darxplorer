with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with DXPL.Compiler.Lexer;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 08/04/2009
--!  Last-Modified :: 08/06/2009

--!  Purpose:
--!  The Input_Manager maintains useful types to handle
--!  the switching between reading from memory or directly
--!  from a file (equivalent to directly touch upon the lexer).
--!
--!  It uses internally a Hashed Map. If you want to 'record'
--!  a procedure into memory, the procedure name will be the
--!  key to the map. The corresponding element to the key is
--!  a list containing all tokens.
----------------------------------------------------------------
generic
	with package Lexer is new DXPL.Compiler.Lexer (<>);
package DXPL.Compiler.Support.Input_Manager is
	package ASU   renames Ada.Strings.Unbounded;

	type Input_Manager is private;
	--  The Input_Manager maintains useful types to handle
	--  the switching between reading from memory or directly
	--  from a file (equivalent to directly touch upon the lexer).
	--
	--  It uses internally a Hashed Map. If you want to 'record'
	--  a procedure into memory, the procedure name will be the
	--  key to the map. The corresponding element to the key is
	--  a list containing all tokens.

	function Get_Input return Input_Type;
	--  Returns the current input type.

	procedure Set_Input (Item : in Input_Type);
	--  Switches between both input types.
	
	procedure Set_Input (Item : in Input_Type; Key : in ASU.Unbounded_String);
	--  Switches to a new input stream by means of the given key.
	
	procedure Add (Key : in ASU.Unbounded_String; Item : in Lexer.Lexical_Vector.Vector);
	--  Adds a new pair of key and item to the internal state.
	
	procedure Replace_Dataset (Item : in Lexer.Lexical_Vector.Vector);
	--  Replaces the current dataset with this new list of lexical elements.

	procedure Reset;
	--  Resets the current stream of lexical elements to the beginning.
	--  By means of this procedure it is possible to iterate over the
	--  same state several times.

	function Has_More_Symbols return Boolean;
	--  Return 'True', if the current stream has more tokens.

	procedure Next_Symbol (Item : in out Lexer.Symbol_Type);
	--  Retrives the next token from list.

	procedure Clear;
	--  Clears the current data

private
	package Lexical_Map is new Ada.Containers.Hashed_Maps
		(Key_Type        => ASU.Unbounded_String,
		 Element_Type    => Lexer.Lexical_Vector.Vector,
		 Hash            => ASU.Hash,
		 Equivalent_Keys => ASU."=",
		 "="             => Lexer.Lexical_Vector."=");
	--  A Lexical Map is a Hashed Map, that contains pairs of
	--  (Unbounded_String, Lexical_Vector.Vector) to emulate
	--  a lexer in memory. The Unbounded String represents a
	--  token read from a file and the corresponding element
	--  in the map keeps track of the state.

	type Input_Manager is
		record
			Current_Input   : Input_Type           := FILE;
			Current_Stream  : ASU.Unbounded_String := ASU.Null_Unbounded_String;
			Current_Vector  : Lexer.Lexical_Vector.Vector;
			Current_Cursor  : Lexer.Lexical_Vector.Extended_Index;
			Lexical_Storage : Lexical_Map.Map;
			Old_Symbol      : Lexer.Symbol_Type;
		end record;
	Input : Input_Manager;
	--  Global instance used to give access to the input manager.

end DXPL.Compiler.Support.Input_Manager;
