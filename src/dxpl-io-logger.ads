with Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;
with DXPL.Booch.Types;	  use DXPL.Booch.Types;
with DXPL.Types;		  use DXPL.Types;
with DXPL.Support.State;  use DXPL.Support.State;
with DXPL.IO.Formatter;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/25/2009
--!  Last-Modified :: 09/18/2009

--!  Purpose:
--!  Supports logging by means of three procedures.
------------------------------------------------------------------------
generic
	F : DXPL.IO.Formatter.Object'Class;
package DXPL.IO.Logger is
	
	-----------------------
	-- Iterative Logging --
	-----------------------

	procedure Initialize (Hashfunction_Name, Algorithm_Name : in String; Current_State : in Round_State);
	--  Starts the logging session

	procedure Finalize;
	--  Finishes the log file (closing the file)

	-------------------------
	-- Graph-based Logging --
	-------------------------

	procedure Initialize;
	--  Starts the logging session. Just clears some internal states.

	procedure Finalize (Hashfunction_Name, Algorithm_Name : in String);
	--  Finishes the log file.

	-------------------------------
	--  Procedures and Functions --
	-------------------------------

	procedure Trace_Stage (Current_State : in Round_State);
	--  Write probablities and more after a stage to the logfile.
	
	procedure Trace_Round (Current_State : in Round_State);
	--  Write probablities and more after a round to the logfile.

	procedure Trace       (Current_State : in Round_State);
	--  Prints all information, which can be extracted from the round state.

	procedure Graph_Based
		(Hashfunction_Name, Name_of_Technique : in String;
		 Last_Vertex : in Directed_Graph.Vertex'Class;
		 Stages      : in Positive);
	--  Allows recursive logging of graph-based algorithms providing
	--  a directed graph for traversal.

------------------
-- Private Part --
-------------------

private
	package IO renames Ada.Text_IO;

	Threshold                   : Conditioned_Float := Conditioned_Float'First;
	Current_Threshold_Candidate : Conditioned_Float := Conditioned_Float'First;
	--  threshold variables keep track of best forward and backward differences

	type Round_Item_Type is abstract tagged
		--  Encapsulates the already known Round_State and
		--  the corresponding hamming weight of the message. This
		--  record could easily enhance the Round_State type, but
		--  then, we had to declare a new Round_Item_Type item
		--  with many parameters.
		record
			Hamming_Weight : Natural := Natural'First;
			State          : Round_State;
		end record;

	procedure Write_Data (Item : in Round_Item_Type; Log_File : in Ada.Text_IO.File_Type) is abstract;
	--  Derivates of Round_Item_Type has to provide this method to write data to the log file.

	type Stage_Record is new Round_Item_Type with null record;
	--  handles round_states gained through stages
	type Round_Record is new Round_Item_Type with null record;
	--  handles round_states gained through rounds
	type Full_Record  is new Round_Item_Type with null record;
	--  handles round_states gained through rounds

	procedure Write_Data (Item : in Stage_Record; Log_File : in Ada.Text_IO.File_Type);
	procedure Write_Data (Item : in Round_Record; Log_File : in Ada.Text_IO.File_Type);
	procedure Write_Data (Item : in Full_Record;  Log_File : in Ada.Text_IO.File_Type);

	package State_Vector is new Ada.Containers.Indefinite_Vectors (Element_Type => Round_Item_Type'Class, Index_Type => Natural);
	Buffer : State_Vector.Vector;
	--  Simplifies storing a round states and eventually the forward and backward iterating

	Log_File  : IO.File_Type;

	Current_Offset : Natural;
	Last_Offset    : Positive := Positive'First;

	Dummy_State     : Round_State;
	Initial_Message : Word_Array := Dummy_State.Message;
	-- Hack to be able to use the non-concrete type 'Word_Array'
	-- The variable 'Dummy_State' is never used.

	File_Counter    : Natural := Natural'First;
	Prepend_Counter : Natural := Natural'First;

	Diff_Output_Format : Ada.Text_IO.Number_Base := 16;
	Diff_Output_Field  : Ada.Text_IO.Field := 22;
	-- intending the words for the output

	Is_Active : Boolean := False;
	Tupel_Log : Boolean := False;
end DXPL.IO.Logger;
