with Ada.Unchecked_Conversion;
with Interfaces;               use Interfaces;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with DXPL.Support.State;

package body DXPL.IO.Debug is
	package Support_State renames DXPL.Support.State;
	
	--------------
	-- Put_Line --
	--------------

	procedure Put_Line (Message : in String) is
	begin
		Ada.Text_IO.Put_Line ("INFO >> " & Message);
	end Put_Line;

	--------------
	-- Put_Line --
	--------------

	procedure Put_Line (Message : in String; Item : in String) is
	begin
		Ada.Text_IO.Put_Line ("DEBUG >> " & Message & " :: " & Item);
	end Put_Line;

	-------------
	-- Support --
	-------------

	procedure Support_A (Probability : in Conditioned_Float) is
	--  This procedure is called by Show_Round and Show_Stage. It provides
	--  a convenient method to convert the given probability to the base 2
	--  into a decimal format.
	begin
		Ada.Text_IO.Put (Convert_Float_To_Decimal_String (Probability));
	end Support_A;

	-------------
	-- Support --
	-------------

	procedure Support_B (Values : in Word_Array) is
		Test : Integer;
	begin
		New_Line;
		for I in Values'Range loop
			Ada.Text_IO.Put_Line (Show_Hex (Values (I)));
		end loop;
	end Support_B;

	--------------
	-- Put_Line --
	--------------

	procedure Put_Line (Current_State : in Round_State; Print_Digest : in Boolean := False) is
	begin
		if not Print_Digest then
			Support_B (Current_State.Message);
		else
			Support_B (Support_State.Get_Digest (Current_State));
		end if;
	end Put_Line;

	--------------
	-- Put_Line --
	--------------

	procedure Put_Line (Item : in Word) is
		Line : String (1 .. Word'Size);
	begin
		M_IO.Put (To => Line, Item => Item, Base => 16);
		Put_Line (Line);
	end Put_Line;

	--------------
	-- Put_Line --
	--------------

	procedure Put_Line (Item : in Word_Array) is
		Line : String (1 .. Word'Size);
	begin
		for I in Item'First .. Item'Last loop
			M_IO.Put (To => Line, Item => Item (I), Base => 16);
			Put_Line (Line);
		end loop;
	end Put_Line;
	----------------
	-- Show_Round --
	----------------

	procedure Show_Round (Current_State     : in Round_State; 
						  Round_Probability : in Conditioned_Float := Conditioned_Float'Last) is
	begin
		Put ("R" & Rounds'Image (Current_State.Round_Offset) & "  full :: P_r := ");
		Support_A (Round_Probability);
		Put (" and P_g := ");
		Support_A (Current_State.Global_Probability);
		Support_B (Current_State.Message);
		New_Line; New_Line; New_Line; New_Line; New_Line;
	end Show_Round;

	----------------
	-- Show_Stage --
	----------------

	procedure Show_Stage (Current_State : in Round_State) is
	begin
		Put ("R" & Rounds'Image (Current_State.Round_Offset) &
			 " | S" & Integer'Image (Current_State.Current_Stage) & " :: P_l := ");
		Support_A (Current_State.Local_Probability);
		Support_B (Current_State.Message);
		New_Line;
	end Show_Stage;

	--------------------
	-- Show_Candidate --
	--------------------

	procedure Show_Candidate (Current_State : in Round_State) is
	begin
		Put ("C" & Rounds'Image (Current_State.Round_Offset) &
			 " | S" & Integer'Image (Current_State.Current_Stage) & " :: P_l := ");
		Support_A (Current_State.Local_Probability);
		Support_B (Current_State.Message);
		New_Line;
	end Show_Candidate;
end DXPL.IO.Debug;
