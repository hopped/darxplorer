with Ada.Strings.Unbounded;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 08/18/2009
--!  Last-Modified :: 08/18/2009

--!  Purpose:
--!  This package maintains the current state of both supported
--!  annotations, 'BEGIN' and 'END'. The procedure Update
--!  takes an access type to an Error_Function.
----------------------------------------------------------------
package DXPL.Compiler.Support.Annotation is
	package ASU renames Ada.Strings.Unbounded;

	type Instance is private;

	------------------
	--  Exceptions  --
	------------------

	Unknown_Annotation           : exception;
	Missing_Annotation           : exception;
	Wrong_Nesting_of_Annotations : exception;

	procedure Update (Obj        : in out Instance;
					  Element    : in ASU.Unbounded_String;
					  Column     : in Positive;
					  Line       : in Positive;
					  Error_Call : in Error_Function);
	--  If Item is equal to 'BEGIN' or 'END', the internal
	--  state will be updated accordingly.

	function Discard_Analyzation (Obj : in Instance) return Boolean;
	--  Returns TRUE, if no 'BEGIN' was read previously.

	function Is_End_Missing (Obj : in Instance) return Boolean;
	--  Returns TRUE, if 'BEGIN' was declared, but
	--  not followed by an 'END' until the end of
	--  a procedure was reached.

	procedure Set_End (Obj : in out Instance; Item : in Boolean);
	--  Manipulates the 'Is_End_Missing' state.

private
	type Instance is
		record
			Discard          : Boolean := True;
			Begin_Is_Missing : Boolean := True;
			End_Is_Missing   : Boolean := False;
		end record;

end DXPL.Compiler.Support.Annotation;
