with Ada.Strings;
with Ada.Strings.Maps.Constants;

package body DXPL.Compiler.Support.Annotation is

	procedure Update (Obj        : in out Instance;
					  Element    : in ASU.Unbounded_String;
					  Column     : in Positive;
					  Line       : in Positive;
					  Error_Call : in Error_Function) is
		use Ada.Strings.Unbounded;
		use Ada.Strings.Maps;
		Item : Unbounded_String := Translate (Element, Ada.Strings.Maps.Constants.Lower_Case_Map);
	begin
		Item := Trim (Item, Ada.Strings.Right);
		if Item = "begin" then
		--  '--# BEGIN' is found
			if not Obj.Begin_Is_Missing then
				Error_Call (Wrong_Nesting_of_Annotations'Identity,
					"Two consecutive 'BEGIN's found. " &
					"See line " & Positive'Image (Line) & ":" & Positive'Image (Column) & ".");
			end if;
			Obj.Discard          := False;
			Obj.Begin_Is_Missing := False;
			Obj.End_Is_Missing   := True;

		--  '--# END' is found
		elsif Item = "end" then
			if not Obj.End_Is_Missing and not Obj.Begin_Is_Missing then
				Error_Call (Wrong_Nesting_of_Annotations'Identity, "Two consecutive 'END's found. " &
					"See line " & Positive'Image (Line) & ":" & Positive'Image (Column) & ".");
			end if;
			Obj.Discard        := True;
			Obj.End_Is_Missing := False;
			if Obj.Begin_Is_Missing then
				Error_Call (Missing_Annotation'Identity, "Annotation 'BEGIN' is missing. " &
					"See line " & Positive'Image (Line) & ":" & Positive'Image (Column) & ".");
			end if;
			Obj.Begin_Is_Missing := True;

		--  Arbitrary annotations are not supported
		else
			Error_Call (Unknown_Annotation'Identity, "The following annotation in line" &
				Positive'Image (Line) & ":" & Positive'Image (Column) &
				" is unknown: " & To_String (Element));
		end if;
	end Update;

	function Discard_Analyzation (Obj : in Instance) return Boolean is
	begin
		return Obj.Discard;
	end Discard_Analyzation;

	procedure Set_End (Obj : in out Instance; Item : in Boolean) is
	begin
		Obj.End_Is_Missing := Item;
	end Set_End;

	function Is_End_Missing (Obj : in Instance) return Boolean is
	begin
		return Obj.End_Is_Missing;
	end Is_End_Missing;

end DXPL.Compiler.Support.Annotation;
