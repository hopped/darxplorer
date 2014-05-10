with Ada.Text_IO;

package body DXPL.Toolbox.Trudy is
	package Support_State renames DXPL.Support.State;

	-------------
	-- Analyse --
	-------------

	procedure Analyse (Technique : in Instance; Current_State : in out Round_State) is
	--  Run the normal anlyzation without additional behaviour.

		Modified_Value : access Word;
		pragma Warnings (Off, Modified_Value);
		pragma Warnings (Off, Technique);
		use Ada.Text_IO;
	begin
		State_Alias := Current_State;

		Put_Line ("INPUT is:");
		Debug.Put_Line (State_Alias);
		New_Line;

		-- Initialize the compression function
		Initialize (State_Alias'Access);

		-- Go through all rounds
		for Current_Round in Rounds'First .. Rounds'Last loop
			State_Alias.Round_Offset := Current_Round;

			-- Go through each stage of processing
			for Current_Stage in Module_Sequence'Range loop
				State_Alias.Current_Stage := Current_Stage;

				-- Compute the current statement represented by a procedure
				Modified_Value := Module_Sequence(Current_Stage)(State_Alias'Access);
			end loop;
		end loop;

		--  Finalize the compression function
		Finalize (State_Alias'Access);
		
		declare
			Digest : Word_Array := Support_State.Get_Digest (State_Alias);
		begin
		--  Copy the current values in 'Message' to the actual variable
		--  holding the digest. If the length of the digest is identical
		--  to 'Message', each value of 'Message' will be overwritten
		--  by iteself. Nothing to worry about...
			for I in Digest'First .. Digest'Last loop
				Digest (I) := State_Alias.Message (I);
				Support_State.Set_Digest (State_Alias, Digest);
			end loop;
		end;

		Put_Line ("OUTPUT is:");
		Debug.Put_Line (State_Alias, True);
		New_Line;

		Current_State := State_Alias;
	end Analyse;

	--------------
	-- Is_Valid --
	--------------

	function Is_Valid (Technique : in Instance) return Boolean is
	--  Simple validation for each test vector. For each test vector,
	--  this function calls 'Analyse'.

		Current_State   : Round_State;
		Number_of_Tests : Positive := Round_Characteristic.Number_of_Tests;
		use Ada.Text_IO;
	begin
		Put_Line ("**  START VALIDATION  **");
		New_Line;
		for I in Positive'First .. Number_Of_Tests loop
			Put_Line ("**  Check test vector no." & Positive'Image (I) & 
				" of" & Positive'Image (Number_Of_Tests) & " ...");
			New_Line;
			Current_State.Message := Round_Characteristic.Get_Message (I);
			Current_State.Key     := Round_Characteristic.Get_Key (I);

			Analyse (Technique, Current_State);

			if not (Support_State.Get_Digest (Current_State) = Round_Characteristic.Get_Digest (I)) then
				return False;
			end if;
			Put_Line ("++  Test vector no." & Positive'Image (I) & 
				" of" & Positive'Image (Number_Of_Tests) & " is valid.");
		end loop;

		New_Line; New_Line; New_Line;

		return True;
	end;
end DXPL.Toolbox.Trudy;
