with DXPL.Round_Function;
with DXPL.Types;			use DXPL.Types;
with DXPL.Toolbox;
with DXPL.Toolbox.Graph;

pragma Elaborate_All (DXPL.Round_Function);
pragma Elaborate_All (DXPL.Toolbox);
pragma Elaborate_All (DXPL.Toolbox.Graph);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of 'Pedantic Petra'.
--!
--!  >>Petra goes a step further than Grete. Given an input 
--!  diﬀerence, she enumerates all output diﬀerences with a nonzero
--!  probability. Recall that this is feasible as long as the Hamming-
--!  weight of the diﬀerence is low. As the number of possible 
--!  diﬀerences is growing fast, and the Hamming-weight also grows 
--!  with the number of rounds, this can be done only for a few rounds 
--!  (say, 7 rounds in each forward and backward direction). Petra is 
--!  pedantic, but not stupid. A naive approach would make her deal 
--!  with a huge number of extremely low-probable characteristics. 
--!  But Petra sorts her intermediate results (characteristics with 
--!  less than the desired number of rounds) by their probability. 
--!  She always proceeds with the most probable intermediate result. 
--!  Speciﬁcally, when she comes to the last round (or the ﬁrst, in 
--!  the case of the backward direction), she just uses Grete’s 
--!  approach.<< [Lucks]
----------------------------------------------------------------
generic
	Number_of_Modules : Integer;
package DXPL.Technique.Petra is

	procedure Compute_Non_Optimal_Outputs
	--  This procedure is valid as a parameter to be used
	--  with the generic round function. Please don't change
	--  the list of parameters.
		 (X, Y				: in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float);

	package Characteristic_Function is new DXPL.Round_Function
	--  Declaration of the round function.
		 (Analyse 			=> Compute_Non_Optimal_Outputs,
		  Number_of_Modules => Number_of_Modules);
		
	package DARX is new Toolbox
	--  Declaration of a concrete toolbox.
		(Round_Characteristic => Characteristic_Function,
		 Toolbox_Name		  => "Pedantic Petra",
		 Toolbox_Threshold    => Toolbox_Threshold,
		 Logging_Formatter    => Default_Formatter);

	package DARXplorer is new DARX.Graph;
	Toolbox : DARXplorer.Instance;
	--  Instance of the toolbox to be used from outside.

end DXPL.Technique.Petra;
