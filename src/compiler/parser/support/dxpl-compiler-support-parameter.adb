with DXPL.Compiler.Debug;

package body DXPL.Compiler.Support.Parameter is
	package Debug renames DXPL.Compiler.Debug;

	function Parameter_Hash (Element : in Parameter_Element) return Ada.Containers.Hash_Type is
	begin
		return ASU.Hash (Element.Value);
	end Parameter_Hash;

	function Equivalent_Parameter (Left, Right : in Parameter_Element) return Boolean is
		use Ada.Strings.Unbounded;
	begin
		return Left.Value = Right.Value and Left.Parameter_Type = Right.Parameter_Type;
	end Equivalent_Parameter;

	function "=" (Left, Right : in Parameter_Element) return Boolean is
		use Ada.Strings.Unbounded;
	begin
		return Left.Value = Right.Value and Left.Parameter_Type = Right.Parameter_Type;
	end "=";

	procedure Add_Argument (Item : ASU.Unbounded_String) is
	begin
		String_Vector.Append (Arguments, Item);
	end Add_Argument;

	procedure Reset_Arguments is
	begin
		String_Vector.Clear (Arguments);
	end Reset_Arguments;

	procedure Add_Parameter
		(Procedure_Name : in ASU.Unbounded_String;
		 Parameter_Name : in ASU.Unbounded_String;
		 Handover_Value : in Handover := In_Value) is
		New_Parameter : Parameter_Element := (Parameter_Name, Handover_Value);
		Elements      : Parameter_Set.Set;
	begin
		if Parameter_Map.Contains (Parameters, Procedure_Name) then
			  --  replace already inserted key
			Elements := Parameter_Map.Element (Parameters, Procedure_Name);
			if not Parameter_Set.Contains (Elements, New_Parameter) then
				Parameter_Set.Insert (Elements, New_Parameter);
				Parameter_Map.Replace (Container => Parameters, Key => Procedure_Name, New_Item => Elements);
			end if;
		else  --  add new key
			if not Parameter_Set.Contains (Elements, New_Parameter) then
				Parameter_Set.Insert (Elements, New_Parameter);
				Parameter_Map.Insert (Container => Parameters, Key => Procedure_Name, New_Item => Elements);
			end if;
		end if;
	end Add_Parameter;

	function Match_Handover (Procedure_Name : in ASU.Unbounded_String) return String is
		Iter_Arguments : String_Vector.Cursor := String_Vector.First (Arguments);
		Set_Parameters : Parameter_Set.Set    := Parameter_Map.Element (Parameters, Procedure_Name);
		Iter_Parameter : Parameter_Set.Cursor := Parameter_Set.First (Set_Parameters);
		Answer         : ASU.Unbounded_String;
		use Ada.Containers;
	begin
		if String_Vector.Length (Arguments) /= Parameter_Set.Length (Set_Parameters) then
			Debug.Trace ("Match_Handover raised Program_Error with  procedure " & ASU.To_String (Procedure_Name));
			Debug.Trace ("Argumente = " & Ada.Containers.Count_Type'Image (String_Vector.Length (Arguments)));
			Debug.Trace ("Parameter = " & Ada.Containers.Count_Type'Image (Parameter_Set.Length (Set_Parameters)));
			raise Program_Error;
		end if;

		while String_Vector.Has_Element (Iter_Arguments) loop
			ASU.Append (Answer, "State.");
			ASU.Append (Answer, ASU.To_String (Parameter_Set.Element (Iter_Parameter).Value));
			ASU.Append (Answer, ":= ");
			ASU.Append (Answer, ASU.To_String (String_Vector.Element (Iter_Arguments)));
			ASU.Append (Answer, "; ");

			String_Vector.Next (Iter_Arguments);
			Parameter_Set.Next (Iter_Parameter);
		end loop;

		return ASU.To_String (Answer);
	end Match_Handover;
	
	function Match_Return (Procedure_Name : in ASU.Unbounded_String) return String is
		Iter_Arguments : String_Vector.Cursor := String_Vector.First (Arguments);
		Set_Parameters : Parameter_Set.Set    := Parameter_Map.Element (Parameters, Procedure_Name);
		Iter_Parameter : Parameter_Set.Cursor := Parameter_Set.First (Set_Parameters);
		Parameter_Item : Parameter_Element;
		Answer         : ASU.Unbounded_String;
		use Ada.Containers;
	begin
		if String_Vector.Length (Arguments) /= Parameter_Set.Length (Set_Parameters) then
			raise Program_Error;
		end if;

		while String_Vector.Has_Element (Iter_Arguments) loop
			Parameter_Item := Parameter_Set.Element (Iter_Parameter);
			if Parameter_Item.Parameter_Type = Out_Value then
				ASU.Append (Answer, ASU.To_String (String_Vector.Element (Iter_Arguments)));
				ASU.Append (Answer, ":= State.");
				ASU.Append (Answer, ASU.To_String (Parameter_Set.Element (Iter_Parameter).Value));
				ASU.Append (Answer, "; ");
			end if;

			String_Vector.Next (Iter_Arguments);
			Parameter_Set.Next (Iter_Parameter);
		end loop;

		return ASU.To_String (Answer);
	end Match_Return;
	
	procedure Clear is
	begin
		Parameter_Map.Clear (Parameters);
		String_Vector.Clear (Arguments);
	end Clear;
end DXPL.Compiler.Support.Parameter;
