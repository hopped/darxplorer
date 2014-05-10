with Ada.Command_Line;
with Ada.Directories;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Calendar_Formatter;
with DXPL.IO.Debug;

package body DXPL.IO.Logger is
	package Debug renames DXPL.IO.Debug;
	package Support_State renames DXPL.Support.State;
	package ASU renames Ada.Strings.Unbounded;
	package M_IO is new  Ada.Text_IO.Modular_IO (Word);
	--  package simplifies output to files

	--------------
	-- Set_Mode --
	--------------

	procedure Set_Mode (Value : Boolean) is
	--  A value of 'True' enables the logging mode. In the result, 
	--  the function 'Do_Logging' returns 'True', too.
	begin
		Is_Active := Value;
	end Set_Mode;
	pragma Inline (Set_Mode);

	----------------
	-- Do_Logging --
	----------------

	function Do_Logging return Boolean is
	--  This functions returns 'True', iff the current logging mode 
	--  is set to true. The variable, which keeps the switch is called
	--  'Is_Active' and can be manipulated by calling 'Set_Mode'.
	begin
	    return Is_Active;
	end Do_Logging;
	pragma Inline (Do_Logging);

	-----------------------
	-- Create_Directory --
	-----------------------

	procedure Create_Directory (Item : in String) is
	--  Creates the directory 'DARX_Logs' to store all results.
	--  If the directory already exists, no error will be raised.
	--  Keep in mind, that older results will not be deleted.
	begin
		if not Ada.Directories.Exists (Item) then
			Ada.Directories.Create_Directory (Item);
		end if;
	end Create_Directory;

	------------------
	-- Print_Number --
	------------------

	function Print_Number (Item : in Integer) return String is
	--  This functions trims the string representation of an
	--  integer to remove the leading whitespace.
	begin
		return Ada.Strings.Fixed.Trim (Integer'Image (Item), Ada.Strings.Left);
	end Print_Number;
	pragma Inline (Print_Number);

	-------------
	-- Replace --
	-------------

	function Replace (From : in Character) return Character is
	--  Replaces a whitespaces by an underscore.
	begin
		if From = ' ' then
			return '_';
		else
			return From;
		end if;
	end;

	------------------------
	-- Remove_Whitespaces --
	------------------------

	function Remove_Whitespaces (Item : in String) return String is
	--  Remove Whitespaces calls a translation method from
	--  Ada.Strings.Fixed to replace whitespaces with underscores
	--  by means of the function 'Replace'.
		White_Range : Ada.Strings.Maps.Character_Range := (' ', ' ');
		Score_Range : Ada.Strings.Maps.Character_Range := ('_', '_');
		Char_Set_A  : Ada.Strings.Maps.Character_Set   := Ada.Strings.Maps.To_Set (White_Range);
		Char_Set_B  : Ada.Strings.Maps.Character_Set   := Ada.Strings.Maps.To_Set (Score_Range);
		Char_Seq_A  : Ada.Strings.Maps.Character_Sequence := Ada.Strings.Maps.To_Sequence (Char_Set_A);
		Char_Seq_B  : Ada.Strings.Maps.Character_Sequence := Ada.Strings.Maps.To_Sequence (Char_Set_B);
		Char_Map    : Ada.Strings.Maps.Character_Mapping  := Ada.Strings.Maps.To_Mapping (Char_Seq_A, Char_Seq_B);
	begin
		return Ada.Strings.Fixed.Translate (Item, Char_Map);
--		return Ada.Strings.Fixed.Translate (Item, Replace'Access);
	end Remove_Whitespaces;

	-----------------
	-- Trace_Round --
	-----------------

	procedure Trace_Round (Current_State : in Round_State) is
	--  Appends a new line to the current buffer
	--  keeping track of round states. 'Trace' should
	--  only be called after executed 'Initialize' once.
	--  Calling 'Trace' after 'Finalize' has no effects.
	begin
		if not Do_Logging then
			return;
		end if;

		declare
		    Item : Round_Record := (Compute_Hamming_Weight (Current_State.Message), Current_State);
		begin
			State_Vector.Append (Buffer, Item);
			Current_Threshold_Candidate := Current_State.Global_Probability;
		end;
	end Trace_Round;

	-----------------
	-- Trace_Stage --
	-----------------

	procedure Trace_Stage (Current_State : in Round_State) is
	--  Appends a new line to the current buffer
	--  keeping track of round states. 'Trace' should
	--  only be called after executed 'Initialize' once.
	--  Calling 'Trace' after 'Finalize' has no effects.
	begin
		if not Do_Logging then
			return;
		end if;

		declare
		    Item : Stage_Record := (Compute_Hamming_Weight (Current_State.Message), Current_State);
		begin
			State_Vector.Append (Buffer, Item);
			Current_Threshold_Candidate := Current_State.Global_Probability;
		end;
	end Trace_Stage;

	-----------
	-- Trace --
	-----------

	procedure Trace (Current_State : in Round_State) is
	--  Appends a new line to the current buffer
	--  keeping track of round states. 'Trace' should
	--  only be called after executed 'Initialize' once.
	--  Calling 'Trace' after 'Finalize' has no effects.
	begin
		if not Do_Logging then
			return;
		end if;

		declare
		    Item : Full_Record := (Compute_Hamming_Weight (Current_State.Message), Current_State);
		begin
			State_Vector.Append (Buffer, Item);
			Current_Threshold_Candidate := Current_State.Global_Probability;
		end;
	end Trace;

	----------------
	-- Write_Data --
	----------------

	procedure Write_Data (Item : in Stage_Record; Log_File : in Ada.Text_IO.File_Type) is
	--  Writes round states and stage states to the given 'Log_File'. Each item will
	--  be handled on the basis of its actual type.
		use DXPL.IO.Formatter;
		Result : String := F.Convert (Item.State, Item.Hamming_Weight, Stage);
	begin
		IO.Put (File => Log_File, Item => Result);
	end Write_Data;

	----------------
	-- Write_Data --
	----------------

	procedure Write_Data (Item : in Round_Record; Log_File : in Ada.Text_IO.File_Type) is
	--  Writes round states and stage states to the given 'Log_File'. Each item will
	--  be handled on the basis of its actual type.
		use DXPL.IO.Formatter;
		Result : String := F.Convert (Item.State, Item.Hamming_Weight, Round);
	begin
		IO.Put (File => Log_File, Item => Result);
	end Write_Data;

	----------------
	-- Write_Data --
	----------------

	procedure Write_Data (Item : in Full_Record; Log_File : in Ada.Text_IO.File_Type) is
	--  Writes round states and stage states to the given 'Log_File'. Each item will
	--  be handled on the basis of its actual type.
		use DXPL.IO.Formatter;
		Result : String := F.Convert (Item.State, Item.Hamming_Weight, Complete);
	begin
		IO.Put (File => Log_File, Item => Result);
	end Write_Data;

	----------------
	-- Initialize --
	----------------

	procedure Initialize (Hashfunction_Name, Algorithm_Name : in String; Current_State : in Round_State) is
	--  Creates the required directory structure to store results. This procedure
	--  also creates the corresponding file for logging. All values are initially set
	--  and written to the created log file. Make sure, that initalize is called
	--  before 'Trace' or 'Finalize'.
		Hamming_Weight : Natural := Natural'First;
		use State_Vector;
	begin
		if not Do_Logging then
			return;
		end if;

		Current_Offset := Current_State.Round_Offset;
		if Current_State.Round_Offset = Rounds'First then
			-- TODO: Current_Offset not of type Rounds. Why set Current_Offset to 0?
			Current_Offset := Natural'First;
		end if;
		
		Initial_Message := Current_State.Message;
		--  Store intial message to recover it while writing statistics
		Prepend_Counter := Natural'First;

		if Current_State.Round_Offset /= Last_Offset then
			--  Reset all global variables, if a new round offset is to be analyzed.
			Last_Offset  := Current_State.Round_Offset;
			File_Counter := Natural'First;
			Threshold                   := Conditioned_Float'First;
			Current_Threshold_Candidate := Conditioned_Float'First;
		end if;

		Create_Directory ("DARX_Logs");
		Create_Directory ("DARX_Logs/" & Remove_Whitespaces (Hashfunction_Name));
		--  creates new global directory to store files

		for Item in Initial_Message'Range loop
			Hamming_Weight := Hamming_Weight + DXPL.Types.Compute_Hamming_Weight (Initial_Message (Item));
		end loop;

		declare
			Filename : String := "./DARX_Logs/" & Remove_Whitespaces (Hashfunction_Name) & "/" &
				Remove_Whitespaces (Algorithm_Name) & "_max" &
				Print_Number (Rounds'Last) & "_start" &
				Print_Number (Current_State.Round_Offset) &  "_hw" &
				Print_Number (Hamming_Weight);
		begin
			Create_Directory (Filename);
			--  creates new subdirectory for this step
			IO.Create (File => Log_File, Name => Filename & "/DARX-Log_" & Print_Number (File_Counter) & ".cvs");
			--  creats new file to store results in
		end;
		File_Counter := Natural'Succ (File_Counter);
		--  update file counter

		IO.Put_Line (File => Log_File, Item => "--!  Copyright 2009;");
		IO.Put_Line (File => Log_File, Item => "--!    Bauhaus-University Weimar, Germany;");
		IO.Put_Line (File => Log_File, Item => "--!    Chair of Media Security / Stefan Lucks;");
		IO.Put_Line (File => Log_File, Item => "--!;");
		IO.Put_Line (File => Log_File, Item => "--!  This file was automatically created due;");
		IO.Put_Line (File => Log_File, Item => "--!  analysation with DARXplorer 2.0. It con-;");
		IO.Put_Line (File => Log_File, Item => "--!  tains information about each stage of;");
		IO.Put_Line (File => Log_File, Item => "--!  processing like the current round, stage,;");
		IO.Put_Line (File => Log_File, Item => "--!  global and local probabilities and differentials.;");
		IO.Put_Line (File => Log_File, Item => "--!;");
		IO.Put_Line (File => Log_File, Item => "--!  DARXplorer 2.0 - Log File;");
		IO.Put_Line (File => Log_File, Item => "--!;");
		declare
			Time : Calendar_Formatter.Formatted_Time := Calendar_Formatter.Get_Time;
		begin
			IO.Put_Line (File => Log_File,Item =>  "--!  Created on: " & Calendar_Formatter.Format_3 (Time) & ";");
		end;
		IO.Put_Line (File => Log_File, Item => "--!;");
		IO.Put_Line (File => Log_File, Item => "--!  Information about the hashfunction;");
		IO.Put_Line (File => Log_File, Item => "--!  Name: " & Support_State.Get_Name & ";");
		IO.Put_Line (File => Log_File, Item => "--!  Technique: " & Algorithm_Name & ";");
		IO.Put_Line (File => Log_File, Item => "--!  # of Rounds:" & Positive'Image (Rounds'Last) & ";");
		IO.Put_Line (File => Log_File, Item => "--!;");
		IO.Put_Line (File => Log_File, Item => "--!  Initial Differential in Round" & Positive'Image (Current_State.Round_Offset) & ";");
		
		declare
			IV_String : ASU.Unbounded_String;
		begin
			for I in Initial_Message'Range loop
			--  initial values written to file
				declare
					Data : String := F.Convert (Initial_Message (I));
				begin
					ASU.Append (IV_String, "--!    " & Data & ";" & ASCII.LF);
				end;
			end loop;
			IO.Put (File => Log_File, Item => ASU.To_String (IV_String));
		end;
		IO.New_Line (Log_File);
		IO.New_Line (Log_File);
	end Initialize;

	--------------
	-- Finalize --
	--------------

	procedure Finalize is
	--  Finalizes the log file by writing best forward and backward differences to the file.

		Counter : Natural := Natural'First;
		Porb_Counter : Natural := Natural'First;

		procedure Write_Difference (Log_File : in Ada.Text_IO.File_Type; Item : in Round_Item_Type'Class) is
		--  Writes differences given by the 'Item' to the given 'Log_File'.
			use State_Vector;
		begin
			IO.Put      (File => Log_File, Item => "Reached Round:");
			Ada.Integer_Text_IO.Put(File => Log_File, Item => Item.State.Round_Offset, Width => 3);
			IO.New_Line (File => Log_File);
			IO.Put      (File => Log_File, Item => "Total Probability: ");
			IO.Put_Line (File => Log_File, Item => Conditioned_Float'Image (Item.State.Global_Probability));
			IO.Put      (File => Log_File, Item => "Round Probability: ");
			IO.Put_Line (File => Log_File, Item => Conditioned_Float'Image (Item.State.Local_Probability));
			IO.Put      (File => Log_File, Item => "Hamming Weight:    ");
			Ada.Integer_Text_IO.Put(File => Log_File, Item => Item.Hamming_Weight, Width => 4);
			IO.New_Line (File => Log_File);
			IO.Put_Line (File => Log_File, Item => "Difference: ");

			for I in Item.State.Message'Range loop
				M_IO.Put    (File => Log_File, Item => Item.State.Message(I), Width => Diff_Output_Field, Base => Diff_Output_Format);
				IO.New_Line (File => Log_File);
			end loop;
		end Write_Difference;

	begin
		if not Do_Logging then
			return;
		end if;

		if not Tupel_Log then
			Porb_Counter := Natural'Succ (Porb_Counter);

			declare
				use State_Vector;
				Buffer_Cursor : Cursor := Buffer.First;
			begin
				if Has_Element (Buffer_Cursor) then
					if Current_Threshold_Candidate > Threshold then
						--  A new best difference was found, that is not logged yet.

						--if Current_Threshold_Candidate > Threshold then
							--  A new best FORWARD difference was found.
							Threshold := Current_Threshold_Candidate;
							--IO.Put_Line (File => Log_File, Item => "This file contains a 'Best Forward Difference';");
							--IO.New_Line (File => Log_File);
							IO.Put_Line (File => Log_File, Item => ";Round; Stage; Global Probability; Local Probability; Hamming Weight; Differentials;");
							IO.New_Line (File => Log_File);
						--end if;

						Buffer_Cursor := Buffer.First;
						--  to be sure, the cursor is set to its first position
						loop
							exit when not Has_Element (Buffer_Cursor);

							Element (Buffer_Cursor).Write_Data (Log_File);
							--Write_Data (Element (Buffer_Cursor), Log_File);
							Next (Buffer_Cursor);
							Counter := Natural'Succ (Counter);

							if Counter = Prepend_Counter then
								IO.New_Line (File => Log_File);
								IO.Put (File => Log_File, Item => "Initial Difference;  ");

								for I in Initial_Message'Range loop
									M_IO.Put (File => Log_File, Item => Initial_Message (I), Base => Diff_Output_Format);
									IO.Put   (File => Log_File, Item => ";  ");
								end loop;
								IO.New_Line (Log_File, 2);
							end if;
						end loop;

						IO.Put_Line (".. logging of " & Ada.Directories.Simple_Name (IO.Name(Log_File)));
						IO.Close  (File => Log_File);
					else
						IO.Delete (File => Log_File);
						File_Counter := Natural'Pred (File_Counter);
					end if;

				else
					IO.Delete (File => Log_File);
					File_Counter := Natural'Pred (File_Counter);
				end if;
			end;
		end if;

		--  # # # # # # # # # # # # # #

		if Tupel_Log then
			declare
				use State_Vector;
				Buffer_Cursor : Cursor := Buffer.First;
			begin
				if Has_Element (Buffer_Cursor) then
					if Current_Threshold_Candidate > Threshold then
						--  A new best difference was found, that is not logged yet.

						--if Current_Threshold_Candidate > Threshold then
							--  A new best FORWARD difference was found.
							Threshold := Current_Threshold_Candidate;
						--	IO.Put_Line (File => Log_File, Item => "This file contains a 'Best Forward Difference';");
						--	IO.New_Line (File => Log_File);
						--end if;

						Buffer_Cursor := Buffer.First;
						if Has_Element (Buffer_Cursor) then
							IO.Put_Line (File => Log_File, Item => "Backwards: ");
							IO.New_Line (File => Log_File);
							--  write the FIRST item of the buffer to the file
							Write_Difference (Log_File, Element (Buffer_Cursor));
						end if;

						IO.New_Line (File => Log_File);

						Buffer_Cursor := Buffer.Last;
						if  Has_Element (Buffer_Cursor) then
							IO.Put_Line (File => Log_File, Item => "Forwards:");
							IO.New_Line (File => Log_File);
							--  write the LAST item of the buffer to the file
							Write_Difference (Log_File, Element (Buffer_Cursor));
						end if;

						IO.Put_Line (".. logging of " & Ada.Directories.Simple_Name (IO.Name(Log_File)));
						IO.Close  (File => Log_File);
					else
						IO.Delete (File => Log_File);
						File_Counter := Natural'Pred (File_Counter);
					end if;

				else
					IO.Delete (File => Log_File);
					File_Counter := Natural'Pred (File_Counter);
				end if;
			end;
		end if;
	end Finalize;

	-----------------
	-- Initialize  --
	-----------------

	procedure Initialize is
	begin
		State_Vector.Clear (Buffer);
		-- clears the buffer to ensure, that no old 
		-- results will conflict with new one
	end Initialize;
	pragma Inline (Initialize);

	--------------
	-- Finalize --
	--------------

    procedure Finalize (Hashfunction_Name, Algorithm_Name : in String) is
    begin
		if not Do_Logging then
			return;
		end if;

		if not Tupel_Log then
		-- and ((Current_Threshold_Candidate  >= Threshold and Most_Rounds_Forward > Round_Forw_Glob)  
		-- or (Current_Threshold_Candidate_Backward > Threshold_Backward and Most_Rounds_Backward <= Rounds_Back_Glob))  then

			Initialize (Hashfunction_Name, Algorithm_Name, State_Vector.Element (Buffer.Last).State);

			if Current_Threshold_Candidate > Threshold then
				--  A new best FORWARD difference was found.
				Threshold := Current_Threshold_Candidate;
				--IO.Put_Line (File => Log_File, Item => "This file contains a 'Best Forward Difference';");
				--IO.New_Line (File => Log_File);
				IO.Put_Line (File => Log_File, Item => ";Round; Stage; Global Probability; Local Probability; Hamming Weight; Differentials;");
				IO.New_Line (File => Log_File);
			--end if;
			
				declare
					use State_Vector;
					Buffer_Cursor : Cursor := Buffer.Last;
				begin
					--  to be sure, the cursor is set to its first position
					loop
						exit when not Has_Element (Buffer_Cursor);

						Element (Buffer_Cursor).Write_Data (Log_File);
						--Write_Data (Element (Buffer_Cursor), Log_File);
						Previous (Buffer_Cursor);
						--Counter := Natural'Succ (Counter);

						--if Counter = Prepend_Counter then
						--	IO.New_Line (File => Log_File);
						--	IO.Put (File => Log_File, Item => "Initial Difference;  ");

						--	for I in Initial_Message'Range loop
						--		M_IO.Put (File => Log_File, Item => Initial_Message (I), Base => Diff_Output_Format);
						--		IO.Put   (File => Log_File, Item => ";  ");
						--	end loop;
						--	IO.New_Line (Log_File, 2);
						--end if;
					end loop;
					declare
						Time : Calendar_Formatter.Formatted_Time := Calendar_Formatter.Get_Time;
					begin
						IO.Put_Line (File => Log_File, Item => "");
						IO.Put_Line (File => Log_File, Item => "--!  Finished at: " & Calendar_Formatter.Format_3 (Time) & ";");
					end;
					IO.Put_Line (".. logging of " & Ada.Directories.Simple_Name (IO.Name(Log_File)));
					IO.Close  (File => Log_File);
				end;
			else
				IO.Delete (File => Log_File);
				File_Counter := Natural'Pred (File_Counter);
			end if;
		end if;
	end Finalize;

	---------------------------
	--- Recursive_Traversal ---
	---------------------------

	procedure Recursive_Traversal (Next_Vertex : in out Directed_Graph.Vertex'Class; Stages : in Positive) is
		Iterator : Generic_Graph.Vertex_Iterator'Class := Directed_Graph.New_Vertex_Outgoing_Iterator (Next_Vertex);
	begin
		while not Generic_Graph.Is_Done (Iterator) loop
			declare
				Next_Arc : Directed_Graph.Arc := Directed_Graph.Arc (Generic_Graph.Current_Arc (Iterator));                  
			begin
				Directed_Graph.To_Vertex (Next_Arc, Next_Vertex);
				declare
					Next_Item : Round_State := Directed_Graph.Item (Next_Vertex);
				begin
				--	Debug.Show_Stage (Next_Item);
					if Directed_Graph.Number_Of_Outgoing_Arcs (Next_Vertex) > 0 then
						if Next_Item.Current_Stage < Stages then
							Trace_Stage (Next_Item);
						else
							Trace_Round (Next_Item);
							Trace_Stage (Next_Item);
						end if;
					end if;
				end;
			end;
			Generic_Graph.Next (Iterator);
		end loop;

		if Directed_Graph.Number_Of_Outgoing_Arcs (Next_Vertex) > 0 then
			Recursive_Traversal (Next_Vertex, Stages);
		end if;
	end Recursive_Traversal;

	-------------------
	--- Graph_Based ---
	-------------------

	procedure Graph_Based (Hashfunction_Name, Name_of_Technique : in String; Last_Vertex : in Directed_Graph.Vertex'Class; Stages : in Positive) is
		--  Enables logging of the results produced by the analyzation.
		--  The logger can be enabled by invoking the darxploer toolbox
		--  in the command line with the parameter '-l'. For a complete
		--  list of possible parameters see DXPL.IO.Logger.
	begin
		if Priority_Graph.Is_Queue_Empty then
		--  No states are listed in the queue. This means, that
		--  the last vertex is the key to trace a path to the root.
			Initialize;
			declare
				Next_Vertex : Directed_Graph.Vertex'Class := Last_Vertex;
			begin
				Trace (Directed_Graph.Item (Next_Vertex));
				--  track the last stage
				Recursive_Traversal (Next_Vertex, Stages);
			end;
			Finalize (Hashfunction_Name, Name_Of_Technique);
		end if;

		loop
			exit when Priority_Graph.Is_Queue_Empty;
			Initialize;
			declare
				Next_Vertex : Directed_Graph.Vertex'Class := Priority_Graph.Dequeue_Next_Best_Node;
				Next_Item   : Round_State := Directed_Graph.Item (Next_Vertex);
				pragma Warnings (Off, Next_Vertex);
			begin
				if Next_Item.Current_Stage < Stages then
					Trace_Stage (Next_Item);
				else
					Trace_Round (Next_Item);
					Trace_Stage (Next_Item);
				end if;
				--  track the last stage
				Recursive_Traversal (Next_Vertex, Stages);
			end;
			Finalize (Hashfunction_Name, Name_of_Technique);
		end loop;
	end Graph_Based;

-------------------------
-- Reading in the Mode --
-------------------------

begin
	for I in 1 .. Ada.Command_Line.Argument_Count loop
		declare
			Arg : String := Ada.Command_Line.Argument (I);
		begin
			if Arg = "--logs" then
				Set_Mode (True);
			elsif Arg = "-l" then
				Set_Mode (True);
			end if;
		end;
	end loop;
end DXPL.IO.Logger;
