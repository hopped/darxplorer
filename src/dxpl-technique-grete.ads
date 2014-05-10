with DXPL.Round_Function;
with DXPL.Toolbox;
with DXPL.Toolbox.Graph;
with DXPL.Types;  use DXPL.Types;

pragma Elaborate_All (DXPL.Round_Function);
pragma Elaborate_All (DXPL.Toolbox);
pragma Elaborate_All (DXPL.Toolbox.Graph);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of 'Greedy Grete'.
--!
--!  Laura just assumes no carry bits are generated. This is 
--!  likely to exclude many 'good' characteristics – a carry bit 
--!  in one round leads to another diﬀerence in another round and 
--!  may ultimately lead to a better (more probable) diﬀerence at 
--!  the end. Grete optimises locally. Given some round’s input 
--!  diﬀerences in some round, she enumerates al l the most probable 
--!  output diﬀerences, using the Lipmaa-Moriai Algorithm for DP+max.
--!  All of them are considered as the input diﬀerence for the next round. 
--!  If there is more than one optimal diﬀerence, Grete’s run time 
--!  will grow exponentially with the number of rounds. Applying Grete 
--!  to 80 rounds is likely to be infeasible, while applying Grete 
--!  to more than 20 rounds (i.e., more than 10 rounds in forward-
--!  direction, and as many rounds in backward-direction) still ought 
--!  to be possible. Observe that Grete disregards all the output 
--!  diﬀerences with a non-optimal probability. But replacing an optimal 
--!  diﬀerence with probability pi = DP+max by a sub-optimal diﬀerence 
--!  with probability p'_i < p_ii may enable the usage of another 
--!  diﬀerence in round j, improving the probability in round j from 
--!  p_j to p'_j.<< [Lucks]
----------------------------------------------------------------
generic
	Number_of_Modules : Integer;
package DXPL.Technique.Grete is

	procedure Compute_All_Optimal_Outputs
	--  This procedure is valid as a parameter to be used
	--  with the generic round function. Please don't change
	--  the list of parameters.
		 (X, Y				: in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float);

	package Characteristic_Function is new DXPL.Round_Function
	--  Declaration of the round function.
		 (Analyse 			=> Compute_All_Optimal_Outputs,
		  Number_of_Modules => Number_of_Modules);
		
	package DARX is new Toolbox
	--  Declaration of a concrete toolbox.
		(Round_Characteristic => Characteristic_Function,
		 Toolbox_Name		  => "Greedy Grete",
		 Toolbox_Threshold    => Toolbox_Threshold,
		 Logging_Formatter    => Default_Formatter);

	package DARXplorer is new DARX.Graph;
	Toolbox : DARXplorer.Instance;
	--  Instance of the toolbox to be used from outside.

end DXPL.Technique.Grete;
