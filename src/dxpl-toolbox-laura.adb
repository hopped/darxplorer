with DXPL.IO.Logger;
with DXPL.IO.Formatter;
with DXPL.IO.Formatter.Default;

package body DXPL.Toolbox.Laura is
	package Support_State renames DXPL.Support.State;

	-------------
	-- Analyse --
	-------------

	procedure Analyse (Technique : in Instance; Current_State : in out Round_State) is
	--  Runs the analyzation.

		Round_Probability  : Conditioned_Float := Conditioned_Float'Last;
		Modified_Value     : access Word;
		pragma Warnings (Off, Modified_Value);
		pragma Warnings (Off, Technique);
	begin
		State_Alias := Current_State;
		-- Initialize the compression function
		Initialize (State_Alias'Access);
		Logger.Initialize;
		Logger.Initialize (Support_State.Get_Name, Image (Technique), State_Alias);

		-- Go through all rounds
		for Current_Round in Current_State.Round_Offset .. Rounds'Last loop
			exit when State_Alias.Global_Probability < Toolbox_Threshold;

			State_Alias.Round_Offset := Current_Round;
			Round_Probability        := Conditioned_Float'Last;

			-- Go through each stage of processing
			for Current_Stage in Module_Sequence'Range loop
				exit when Round_Probability < Toolbox_Threshold;

				State_Alias.Current_Stage := Current_Stage;
				-- Compute the current statement represented by a procedure
				Modified_Value    := Module_Sequence(Current_Stage)(State_Alias'Access);
				Round_Probability := Round_Probability * State_Alias.Local_Probability;

				Debug.Show_Stage (State_Alias);
				Logger.Trace_Stage (State_Alias);
			end loop;

			State_Alias.Global_Probability := State_Alias.Global_Probability * Round_Probability;

			Logger.Trace_Round (State_Alias);
			Debug.Show_Round (State_Alias, Round_Probability);
		end loop;

		-- Finalization occurs only, if the last round and the last stage were processed
		if State_Alias.Round_Offset = Rounds'Last and State_Alias.Current_Stage = Module_Offset'Last then
			Finalize (State_Alias'Access);
		end if;

		Logger.Finalize;

		Current_State := State_Alias;
	end Analyse;

end DXPL.Toolbox.Laura;
