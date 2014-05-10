with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body DXPL.IO.Formatter.Default is
	package ASU renames Ada.Strings.Unbounded;
	package FIO is new Ada.Text_IO.Float_IO (Conditioned_Float);

	-------------
	-- Convert --
	-------------

	function Convert (Obj : in Instance; Item : in Conditioned_Float) return String is
		Aft    : Positive := 4;
		Result : ASU.Unbounded_String;
		Value  : String (1 .. 20);
		--  fail-safe
	begin
		FIO.Put (Value, Item, Aft, FIO.Default_Exp);
		ASU.Append (Result, Value);
		ASU.Append (Result, "  [");
		ASU.Append (Result, Convert_Float_To_Decimal_String (Item));
		ASU.Append (Result, "]");
		return ASU.To_String (Result);
	end Convert;

	-------------
	-- Convert --
	-------------

	function Convert (Obj : in Instance; Item : in Word) return String is
	begin
		return Show_Hex (Item);
	end Convert;

	-------------
	-- Convert --
	-------------

	function Convert (Obj : in Instance; Item : in Word_Array)
	  return String is
	  Result : ASU.Unbounded_String;
	begin
		for I in Item'Range loop
			ASU.Append (Result, Show_Hex (Item(I)));
		end loop;
		
		return ASU.To_String (Result);
	end Convert;

	-------------
	-- Convert --
	-------------

	function Convert (Obj   : in Instance;
					  Item  : in Round_State;
					  HW    : in Natural;
					  State : in Level) return String is
		use ASU;
		Data : Unbounded_String;
	begin
		Append (Data, ASCII.LF);
		--  common data
		case State is
			when Stage =>
				Append (Data, ";" & Integer'Image (Item.Round_Offset) & ";");
				Append (Data, Integer'Image (Item.Current_Stage) & ";");
				Append (Data, "     ; ");
				Append (Data, Convert (Obj, Item.Local_Probability) & ";");

			when Round =>
				Append (Data, "Round #" & Integer'Image (Item.Round_Offset) & " completed;" & ASCII.LF);
				Append (Data, ";" & Integer'Image (Item.Round_Offset) & ";");
				Append (Data, "     ; ");
				Append (Data, Convert (Obj, Item.Global_Probability) & ";");
				Append (Data, "     ; ");

			when Complete =>
				Append (Data, ";" & Integer'Image (Item.Round_Offset) & ";");
				Append (Data, Integer'Image (Item.Current_Stage) & ";");
				Append (Data, Convert (Obj, Item.Global_Probability) & ";");
				Append (Data, Convert (Obj, Item.Local_Probability) & ";");
		end case;

		Append (Data, Integer'Image (HW) & "; ");
		Append (Data, ASCII.LF);
		--  differentials
		Append (Data, ";;;;;;");
		Append (Data, Convert (Obj, Item.Message(Item.Message'First)));
		Append (Data, "; " & ASCII.LF);
		for I in Item.Message'First .. Item.Message'Last - 1 loop
			Append (Data, ";;;;;;");
			Append (Data, Convert (Obj, Item.Message(I)) & "; ");
			Append (Data, ASCII.LF);
		end loop;
		Append (Data, ASCII.LF);
		
		return To_String (Data);
	end Convert;

end DXPL.IO.Formatter.Default;
