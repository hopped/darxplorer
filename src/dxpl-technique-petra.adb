with DXPL.Support.Differential_Addition;  use DXPL.Support.Differential_Addition;

package body DXPL.Technique.Petra is

	---------------------------------
	-- Compute_Non_Optimal_Outputs --
	---------------------------------

	procedure Compute_Non_Optimal_Outputs
		 (X                 : in     Word;
		  Y                 : in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float) is
		pragma Warnings (Off, Z);
	begin
		All_Non_Optimal_Differentials (X, Y, Output_Candidates);
		Differential_Probability (X, Y, Word_Vector.First_Element (Output_Candidates), Probability);
	end Compute_Non_Optimal_Outputs;

end DXPL.Technique.Petra;