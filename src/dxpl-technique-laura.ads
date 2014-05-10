with DXPL.Round_Function;
with DXPL.Toolbox;
with DXPL.Toolbox.Laura;
with DXPL.Types;  use DXPL.Types;

pragma Elaborate_All (DXPL.Round_Function);
pragma Elaborate_All (DXPL.Toolbox);
pragma Elaborate_All (DXPL.Toolbox.Laura);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/30/2009
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of 'Lazy Laura'.
--!
--!  >>Laura just 'guesses' that the the addition behaves like the 
--!  xor, diﬀerence-wise. I.e., there are no carry bits generated. 
--!  Starting with low Hamming-weight diﬀerences in the middle, it 
--!  is very easy for her to compute the input diﬀerence (running  
--!  as many rounds of Threeﬁsh as needed backwards), and to 
--!  compute the output diﬀerence (running Threeﬁsh in forward 
--!  direction). Given any characteristic, it is easy to compute 
--!  its probability by employing the Lipmaa-Moriai algorithm for 
--!  DP+. The beneﬁt of Laura’s approach is that it is very efficient
--!  and can be implemented for any number of rounds (for 80 rounds, 
--!  or for 800 rounds . . . ).<< [Lucks]
----------------------------------------------------------------
generic
	Number_of_Modules : Integer;
package DXPL.Technique.Laura is

	procedure Compute_Optimal_Output
	--  This procedure is valid as a parameter to be used
	--  with the generic round function. Please don't change
	--  the list of parameters.
		 (X, Y				: in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float);

	package Characteristic_Function is new DXPL.Round_Function
	--  Declaration of the round function.
		 (Analyse 			=> Compute_Optimal_Output,
		  Number_of_Modules => Number_of_Modules);

	package DARX is new DXPL.Toolbox
	--  Declaration of a concrete toolbox.
		(Round_Characteristic => Characteristic_Function,
		 Toolbox_Name		  => "Lazy Laura",
		 Toolbox_Threshold    => Toolbox_Threshold,
		 Logging_Formatter    => Default_Formatter);

	package DARXplorer is new DARX.Laura;
	Toolbox : DARXplorer.Instance;
	--  Instance of the toolbox to be used from outside.

end DXPL.Technique.Laura;