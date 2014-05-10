with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 08/06/2009
--!  Last-Modified :: 08/06/2009

--!  Purpose:
--!  This packages encaspulates all information needed to
--!  create the required file for the general darXplorer-
--!  framework, that defines the round state.
--!
--!  You can add an arbitrary number of variables to define
--!  the round state. By means of the procedure 'Trace', 
--!  the gathered information is written as a ADA specification
--!  file to the hard drive.
----------------------------------------------------------------
package DXPL.Compiler.Support.State is
	package ASU renames Ada.Strings.Unbounded;

	type Instance is null record;

	procedure Add (Object : in Instance; Key : in ASU.Unbounded_String; Item : in ASU.Unbounded_String);
	--  Checks, if the given key (an identifier for a variable) is already kept
	--  in state. If not, the key with the corresponding item (complete
	--  declaration like 'Item : Boolean := False') will be added to the round state.

	procedure Set_Key_Support (Object : in Instance; Item : in Boolean);
	--  Takes a boolean value as argument to define, if keys are used
	--  in the description of a hash function or not. If not, we need to
	--  provide a dummy variable for the purpose of compatibility with
	--  respect to the general framework.

	function Contains (Object : in Instance; Key : in ASU.Unbounded_String) return Boolean;
	--  Returns true, iff the key (variable name) is already in the state.

	procedure Trace (Object : in Instance; Directory, Filename, Name : in String);
	--  Iterates over the variables, that were added to the state and
	--  writes the file named by the given parameters. Finally, the file is closed.

private
	Keys_Declared : Boolean := False;
	--  Variable is used to differentiate between two states:
	--  True  ::= Keys are declared by the hash function
	--  False ::= No keys were declared

	package Strings_Map is new Ada.Containers.Hashed_Maps
	--  A map keeps track of variables and their full declaration.
		(Key_Type        => ASU.Unbounded_String,
		 Element_Type    => ASU.Unbounded_String,
		 Hash            => ASU.Hash,
		 Equivalent_Keys => ASU."=",
		 "="             => ASU."=");
	State : Strings_Map.Map;

end DXPL.Compiler.Support.State;
