with Ada.Strings.Fixed;

package body DXPL.Compiler.Support.Process_Procedure is

	function Get_Next_Declaration return ASU.Unbounded_String is
		use Ada.Strings.Unbounded;

		-- remove leading space generated automatically by conversion from positive to string
		Str_Count   : String := Ada.Strings.Fixed.Delete (Positive'Image (State.Count), 1, 1);
		Declaration : ASU.Unbounded_String := State.Identifier & Str_Count & State.Parameter;
	begin
		State.Declaration := Declaration;
		State.Count := Positive'Succ (State.Count);
		
		return State.Declaration;
	end Get_Next_Declaration;

	function Get_Declaration return ASU.Unbounded_String is
	begin
		return State.Declaration;
	end Get_Declaration;

	function Get_Identifier return ASU.Unbounded_String is
	begin
		return ASU.To_Unbounded_String (State.Identifier);
	end Get_Identifier;

	function Get_Count return Positive is
	begin
		return State.Count;
	end Get_Count;

	function Get_Parameter return ASU.Unbounded_String is
	begin
		return State.Parameter;
	end Get_Parameter;

	procedure Set_Parameter (Item : in ASU.Unbounded_String) is
	begin
		State.Parameter := Item;
	end Set_Parameter;

	procedure Clear is
	begin
		State.Count := 1;
		State.Parameter   := ASU.Null_Unbounded_String;
		State.Declaration := ASU.Null_Unbounded_String;
	end Clear;
end DXPL.Compiler.Support.Process_Procedure;
