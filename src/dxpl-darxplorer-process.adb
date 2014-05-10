with Calendar_Formatter;
with DXPL.Booch.Priority_Queue;
with Ada.Containers.Vectors;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO;
with DXPL.Technique;
with DXPL.Technique.Grete;
with DXPL.Technique.Laura;
with DXPL.Technique.Petra;
with DXPL.Technique.Trudy;
with DXPL.Support.State; use DXPL.Support.State;

separate (DXPL.DARXPLORER)

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 08/19/2009
--!  Last-Modified :: 09/12/2009

--!  Purpose:
--!  Separate package of DXPL.DARXPLORER, which will be
--!  be created automatically by the compiler. The procedure
--!  Process is purely static. It allows the execution of
--!  a desired analyzation technique by means of an argument
--!  passing per command line like '-trudy', '-laura', etc.
--!  All techiques but Trusty Trudy need a second argument
--!  to declare the hamming weight for the batch job.
----------------------------------------------------------------
procedure Process is
	package ASU renames Ada.Strings.Unbounded;

	Technique : String := "trudy";
	H_Weight  : Hamming_Weight := HW_1;
	Finished  : exception;

	procedure Trusty_Trudy (Threshold : in Conditioned_Float; Number_of_Modules : in Integer) is
	--  Generates the required packages to allow the validation of
	--  the given compression function by means of Trusty Trudy.
	--  DARXPLORER will tell the user, if the description was correct or not.
		package Generic_Technique is new DXPL.Technique (Threshold);
		package Current_Technique is new Generic_Technique.Trudy (Number_of_Modules);
		Result : Boolean := Current_Technique.Toolbox.Is_Valid;
	begin
		Ada.Text_IO.Put ("Your description of " & Name & " is ");
		if Result then
			Ada.Text_IO.Put_Line ("valid. (TRUE)");
		else
			Ada.Text_IO.Put_Line ("NOT valid. (FALSE)");
		end if;
	end Trusty_Trudy;

	procedure Lazy_Laura (Threshold         : in Conditioned_Float;
						  Number_of_Modules : in Integer;
						  H_Weight          : in Hamming_Weight) is
	--  Generates the required packages to allow the analyzation of
	--  the given compression function by means of Lazy_Laura.
		package Generic_Technique is new DXPL.Technique (Threshold);
		package Current_Technique is new Generic_Technique.Laura (Number_of_Modules);
	begin
		Current_Technique.Toolbox.Batch_Analyse (H_Weight);
	end Lazy_Laura;

	procedure Greedy_Grete (Threshold         : in Conditioned_Float;
							Number_of_Modules : in Integer;
							H_Weight          : in Hamming_Weight) is
	--  Generates the required packages to allow the analyzation of
	--  the given compression function by means of Greedy_Grete.
		package Generic_Technique is new DXPL.Technique (Threshold);
		package Current_Technique is new Generic_Technique.Grete (Number_of_Modules);
	begin
		Current_Technique.Toolbox.Batch_Analyse (H_Weight);
	end Greedy_Grete;

	procedure Pedantic_Petra (Threshold         : in Conditioned_Float;
							  Number_of_Modules : in Integer;
							  H_Weight          : in Hamming_Weight) is
	--  Generates the required packages to allow the analyzation of
	--  the given compression function by means of Pedantic_Petra.
		package Generic_Technique is new DXPL.Technique (Threshold);
		package Current_Technique is new Generic_Technique.Petra (Number_of_Modules);
	begin
		Current_Technique.Toolbox.Batch_Analyse (H_Weight);
	end Pedantic_Petra;

	procedure Fuzzy_Fiona (Threshold         : in Conditioned_Float;
						   Number_of_Modules : in Integer;
						   H_Weight          : in Hamming_Weight) is
	--  Generates the required packages to allow the analyzation of
	--  the given compression function by means of Fuzzy_Fiona.
	--	package Generic_Technique is new DXPL.Technique (Threshold);
	--	package Current_Technique is new Generic_Technique.Fiona (Number_of_Modules);
	begin
		Ada.Text_IO.Put_Line ("Not Implemented");
	end Fuzzy_Fiona;

	procedure Print_Help (Filename : in String) is
	--  Reads in README.txt, if available, and print it to the screen.
	--  If README.txt cannot be found, print standard usage information.
		use Ada.Text_IO;
	begin
		Put_Line ("Usage: darxplorer [OPTION...]");
		New_Line;
		Put_Line ("Main modes of operation:");
		Put_Line ("  -t, --technique=TECHNIQUE   choose a technique for analyzation");
		Put_Line ("                              (1) TECHNIQUE=laura");
		Put_Line ("                              (2) TECHNIQUE=fiona");
		Put_Line ("                              (3) TECHNIQUE=grete");
		Put_Line ("                              (4) TECHNIQUE=petra");
		Put_Line ("                              (5) TECHNIQUE=trudy");
		Put_Line ("  -h, --hamming_weight=DIGIT  chose a hamming weight between 1 and 4");
		New_Line;
		Put_Line ("Controlling output:");
		Put_Line ("  -l, --logs                  enable logging while analyzing");
		New_Line;
		Put_Line ("Mandatory or optional arguments to long options are also mandatory");
		Put_Line ("or optional for any corresponding short options.");
		New_Line;
		Put_Line ("Report bugs to dennis.hoppe@uni-weimar.de.");
	end Print_Help;

	procedure Print_Usage is
		use Ada.Text_IO;
	begin
		Put_Line ("Usage: darxplorer [-l] [-t=TECHNIQUE] [-h=HAMMING_WEIGHT]");
		Put_Line ("                  [--help][--usage] [--version]");
		raise Finished;
	end Print_Usage;

	procedure Print_Version is
	begin
		Ada.Text_IO.Put_Line ("DARXplorer 2.0");
	end Print_Version;

	package Unbounded_Vector is new Ada.Containers.Vectors
		(Element_Type => ASU.Unbounded_String, Index_Type => Positive, "=" => ASU."=");

	procedure Handle_Single_Arguments (Arguments : in Unbounded_Vector.Vector) is
		use Unbounded_Vector;
		use ASU;
		Item  : Unbounded_String := First_Element (Arguments);
		Value : Unbounded_String := Translate(Item, Ada.Strings.Maps.Constants.Lower_Case_Map);
	begin
	    Value := ASU.Trim (Value, Ada.Strings.Right);
		if Value = "--help" then
			Print_Help ("README.txt");
		elsif Value = "--usage" then
			Print_Usage;
		elsif Value = "--version" then
			Print_Version;
		else
			Print_Usage;
		end if;
	end Handle_Single_Arguments;

	function Split (Source_String : in String; Split : in String)
	  return Unbounded_Vector.Vector is
	--  taken from http://rosettacode.org/wiki/Tokenizing_A_String#Ada
		use Ada.Strings.Fixed;
		Index_List : array(1..256) of Natural;
		Next_Index : Natural := 1;
		RetVal : Unbounded_Vector.Vector;
		use Unbounded_Vector;
	begin
		Index_List(Next_Index) := 1;
		while Index_List(Next_Index) < Source_String'Last loop
			Next_Index := Next_Index + 1;
			Index_List(Next_Index) := 1 + Index(Source_String(Index_List(Next_Index - 1)..Source_String'Last), Split);
			if Index_List(Next_Index) = 1 then 
				Index_List(Next_Index) := Source_String'Last + 2;
			end if;
			Append (RetVal, ASU.To_Unbounded_String (Source_String(Index_List(Next_Index - 1)..Index_List(Next_Index)-2)));
		end loop;

		return RetVal; 
	end Split;

	procedure Handle_Single_Element (Arguments : in Unbounded_Vector.Vector) is
		use Unbounded_Vector;
		Argument : ASU.Unbounded_String := First_Element (Arguments);
		use ASU;
	begin
		Argument := ASU.Translate(Argument, Ada.Strings.Maps.Constants.Lower_Case_Map);
		Argument := ASU.Trim (Argument, Ada.Strings.Right);
		if Argument = "-l" or Argument = "--logs" then
			--  Handle the case, a user wants to log the results.
			--  Possible parameters for logging can be looked up 
			--  in DXPL.IO.Logger. The logger is integerated in 
			--  the DXPL.Toolbox child packages and no further
			--  initialization is needed at this point.
			null;
		else
			Print_Usage;
		end if;
	end Handle_Single_Element;

	procedure Handle_Pairs (Arguments : in Unbounded_Vector.Vector; Sole : in Boolean := False) is
		use Unbounded_Vector;
		use ASU;
		Key   : ASU.Unbounded_String := First_Element (Arguments);
		Value : ASU.Unbounded_String := Last_Element (Arguments);
	begin
		Key   := ASU.Translate(Key,   Ada.Strings.Maps.Constants.Lower_Case_Map);
		Value := ASU.Translate(Value, Ada.Strings.Maps.Constants.Lower_Case_Map);
		Value := ASU.Trim (Value, Ada.Strings.Right);
		if Key = "-t" or Key = "--technique" then
			if Value = "laura" or Value = "grete" or
			   Value = "petra" or Value = "fiona" or Value = "trudy" then
				Technique := To_String (Value);
			else
				Print_Usage;
			end if;

		elsif not Sole and (Key = "-h" or Key = "--hamming_weight") then
			if    Value = "1" then
				H_Weight := HW_1;
			elsif Value = "2" then
				H_Weight := HW_2;
			elsif Value = "3" then
				H_Weight := HW_3;
			elsif Value = "4" then
				H_Weight := HW_4;
			else
				Print_Usage;
			end if;
		else
			Print_Usage;
		end if;
	end Handle_Pairs;

	Time : Calendar_Formatter.Formatted_Time := Calendar_Formatter.Get_Time;
	use ASU;
begin
	if Ada.Command_Line.Argument_Count < Positive'First then
		Ada.Text_IO.Put_Line ("DARXplorer 2.0 with support for " & Name & ".");
		raise Finished;
	end if;

	declare
		Length : Positive := Ada.Command_Line.Argument_Count;
	begin
		if Length = 1 then
			declare
				Arguments : Unbounded_Vector.Vector := Split (Ada.Command_Line.Argument (Positive'First) & " ", "=");
				use type Ada.Containers.Count_Type;
			begin
				if Unbounded_Vector.Length (Arguments) = 1 then
					Handle_Single_Arguments (Arguments);
					raise Finished;

				elsif Unbounded_Vector.Length (Arguments) = 2 then
					Handle_Pairs (Arguments, True);

				end if;
			end;
		end if;

		for ARGC in Positive'First .. Length loop
			declare
				Arguments : Unbounded_Vector.Vector := Split (Ada.Command_Line.Argument (ARGC) & " ", "=");
				use type Ada.Containers.Count_Type;
			begin
				if Unbounded_Vector.Length (Arguments) = 1 then
					Handle_Single_Element (Arguments);

				elsif Unbounded_Vector.Length (Arguments) = 2 then
					Handle_Pairs (Arguments);

				end if;
			end;
		end loop;

		Ada.Text_IO.Put_Line ("** DARXPLORER 2.0 **");
		Ada.Text_IO.Put_Line ("-- Framework based on description of " & Name & "--");
		Ada.Text_IO.Put_Line ("-- Analyzation started at " & Calendar_Formatter.Format_3 (Time) & " --");
		Ada.Text_IO.New_Line;

		if    Technique = "laura" then
			Lazy_Laura (Threshold, Number_of_Modules, H_Weight);
		elsif Technique = "petra" then
			Pedantic_Petra (Threshold, Number_of_Modules, H_Weight);
		elsif Technique = "grete" then
			Greedy_Grete (Threshold, Number_of_Modules, H_Weight);
		elsif Technique = "trudy" then
			Trusty_Trudy (Threshold, Number_of_Modules);
		elsif Technique = "fiona" then
			Fuzzy_Fiona (Threshold, Number_of_Modules, H_Weight);
		end if;

		Ada.Text_IO.New_Line;
		Time := Calendar_Formatter.Get_Time;
		Ada.Text_IO.Put_Line ("-- Analyzation stopped at " & Calendar_Formatter.Format_3 (Time) & " --");
		Ada.Text_IO.Put_Line ("** FINISHED **");
	end;

	exception when Finished =>
		null;
end Process;
