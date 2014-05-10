with DXPL.Support.Differential_Addition;  use DXPL.Support.Differential_Addition;

package body DXPL.Technique.Grete is

	---------------------------------
	-- Compute_All_Optimal_Outputs --
	---------------------------------

	procedure Compute_All_Optimal_Outputs
		 (X, Y				: in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float) is
		pragma Warnings (Off, Z);
	begin
		All_Optimal_Differentials (X, Y, Output_Candidates);
		Differential_Probability  (X, Y, Word_Vector.First_Element (Output_Candidates), Probability);
		-- compute probability (each output yields to the same probability)
	end Compute_All_Optimal_Outputs;

end DXPL.Technique.Grete;