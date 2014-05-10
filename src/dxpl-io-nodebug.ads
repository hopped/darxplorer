with DXPL.Types;		  use DXPL.Types;
with DXPL.Support.State;  use DXPL.Support.State;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/25/2009
--!  Last-Modified :: 09/23/2009

--!  Purpose:
--!  Implementation of dummy procedure to allow switching bet-
--!  debugging and no debugging. Here, no debugging functionali-
--!  ty is provided. For more information see DXPL.IO.Debug.
----------------------------------------------------------------
package DXPL.IO.NoDebug is

	procedure Put_Line (Message : in String) is null;
	--  Shows a info message like INFO>> 'Message'

	procedure Put_Line (Message : in String; Item : in String) is null;
	--  Shows a debug message like DEBUG>> 'Message' :: 'Item'

	procedure Put_Line
		(Current_State : in Round_State;
		 Print_Digest  : in Boolean := False) is null;
	--  Prints the current message to the command line.

	procedure Put_Line (Item : in Word) is null;
	--  Prints out a 'Word' by means of a hexadecimal representation

	procedure Put_Line (Item : in Word_Array) is null;
	--  Prints out a 'Word_Array' by means of a hexadecimal representation

	procedure Show_Round 
		(Current_State     : in Round_State;
		 Round_Probability : in Conditioned_Float := Conditioned_Float'Last) is null;
	--  Provides a convenient method to show a complete round

	procedure Show_Stage (Current_State : in Round_State) is null;
	--  Provides a convenient method to show a complete stage

	procedure Show_Candidate (Current_State : in Round_State) is null;

end DXPL.IO.NoDebug;
