package body DXPL.Technique.Trudy is

	-----------
	-- Solve --
	-----------

	procedure Solve
		 (X, Y				: in     Word;
		  Z					: in out Word;
		  Output_Candidates : in out Word_Vector.Vector;
		  Probability       : in out Conditioned_Float) is
		pragma Warnings (Off, Output_Candidates);
		pragma Warnings (Off, Probability);
	begin
		Z := X + Y;
	end Solve;

end DXPL.Technique.Trudy;