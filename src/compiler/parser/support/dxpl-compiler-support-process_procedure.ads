with Ada.Strings.Unbounded;

-------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 10/27/2009

--!  Purpose:
--!  Just magic! :)
-------------------------------------------------------------------
package DXPL.Compiler.Support.Process_Procedure is
	package ASU renames Ada.Strings.Unbounded;

	type Process_Declaration_Type is limited private;

	function Get_Next_Declaration return ASU.Unbounded_String;

	function Get_Declaration return ASU.Unbounded_String;
	pragma Inline (Get_Declaration);

	function Get_Identifier return ASU.Unbounded_String;

	function Get_Count return Positive;

	function Get_Parameter return ASU.Unbounded_String;

	procedure Set_Parameter (Item : in ASU.Unbounded_String);

	procedure Clear;
private
	type Process_Declaration_Type is
		record
			Identifier  : String (1 .. 17)    := "function Process_";
			Count       : Positive            := 1;
			Parameter   : ASU.Unbounded_String := ASU.Null_Unbounded_String;
			Declaration : ASU.Unbounded_String := ASU.Null_Unbounded_String;
		end record;
	State : Process_Declaration_Type;

end DXPL.Compiler.Support.Process_Procedure;
