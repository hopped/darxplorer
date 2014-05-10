with DXPL.Support.Differential_Addition;  use DXPL.Support.Differential_Addition;

package body DXPL.Technique.Laura is

	---------
	-- "+" --
	---------

	function "+" (L, R : Word) return Word is
	begin
		return L xor R;
	end "+";

	----------------------------
	-- Compute_Optimal_Output --
	----------------------------

	procedure Compute_Optimal_Output
		 (X, Y				: in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float) is
		pragma Warnings (Off, Output_Candidates);
	begin
		Z := X + Y;
		Differential_Probability (X, Y, Z, Probability);
	end Compute_Optimal_Output;

end DXPL.Technique.Laura;