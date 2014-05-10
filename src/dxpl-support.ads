with DXPL.Types;  use DXPL.Types;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  Definition of an essential record to keep track of the 
--!  current state of a hash function. The state comprises
--!  amongst other things the current round number, the
--!  stage number and output differences. It will be further
--!  enhanced by means of DXPL.Support.State to include
--!  the actual message.
----------------------------------------------------------------
package DXPL.Support is
	type Basic_State is tagged
		record
			--  Name of the hash function
			Local_Probability  : Conditioned_Float := Conditioned_Float'Last;
			--  Represents the probability for a 'stage-based' step
			Global_Probability : Conditioned_Float := Conditioned_Float'Last;
			--  Indicates a probability for a whole round
			Round_Offset       : Rounds   := Rounds'First;
			--  The current round of a hash function.
			Current_Stage      : Positive := Positive'First; 
			--  The momentary stage currently processed
			--  PREFERRED: Current_State : Module_Offset := Module_Offset'First;
			Output_Candidates  : Word_Vector.Vector := Word_Vector.Empty_Vector;
			--  Output differentials gained from processing a stage expression.
		end record;
end DXPL.Support;