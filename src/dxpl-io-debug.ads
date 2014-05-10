with Ada.Text_IO;
with DXPL.Types;		  use DXPL.Types;
with DXPL.Support.State;  use DXPL.Support.State;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/25/2009
--!  Last-Modified :: 09/12/2009

--!  Purpose:
--!  Representation of a simple debug package. It provides
--!  meaningful procedures to output simple messages and even
--!  complete round states. Output will be passed to the
--!  command line per default.
----------------------------------------------------------------
package DXPL.IO.Debug is

	-------------------------------
	-- Functions and Procedures --
	-------------------------------

	procedure Put_Line (Message : in String);
	--  Shows a info message like INFO>> 'Message'

	procedure Put_Line (Message : in String; Item : in String);
	--  Shows a debug message like DEBUG>> 'Message' :: 'Item'

	procedure Put_Line
		(Current_State : in Round_State;
		 Print_Digest  : in Boolean := False);
	--  Prints the current message to the command line.

	procedure Put_Line (Item : in Word);
	--  Prints out a 'Word' by means of a hexadecimal representation

	procedure Put_Line (Item : in Word_Array);
	--  Prints out a 'Word_Array' by means of a hexadecimal representation

	procedure Show_Round 
		(Current_State     : in Round_State;
		 Round_Probability : in Conditioned_Float := Conditioned_Float'Last);
	--  Provides a convenient method to show a complete round

	procedure Show_Stage (Current_State : in Round_State);
	--  Provides a convenient method to show a complete stage

	procedure Show_Candidate (Current_State : in Round_State);

------------------
-- Private Part --
------------------

private
	package M_IO is new Ada.Text_IO.Modular_IO (Word);
	package F_IO is new Ada.Text_IO.Float_IO   (Conditioned_Float);

	use Ada.Text_IO;
	procedure New_Line (Spacing : in Positive_Count := 1) renames Ada.Text_IO.New_Line;
end DXPL.IO.Debug;
