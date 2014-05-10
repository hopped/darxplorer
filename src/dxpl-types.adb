with Interfaces; use Interfaces;

package body DXPL.Types is

	----------------------------
	-- Compute_Hamming_Weight --
	----------------------------

	function Compute_Hamming_Weight (Value : in Word) return Natural is
		Weight : Unsigned_64 := Unsigned_64'Val(Value);
	begin
		Weight := Weight - (Shift_Right(Weight, 1) and 16#5555_5555_5555_5555#);
		Weight := (Weight and 16#3333_3333_3333_3333#) + (Shift_Right(Weight, 2) and 16#3333_3333_3333_3333#);
		Weight := (Weight + Shift_Right(Weight, 4)) and 16#0f0f_0f0f_0f0f_0f0f#;

		return Natural'Val(Shift_Right(Weight * 16#0101_0101_0101_0101#, 56));
	end Compute_Hamming_Weight;

	----------------------------
	-- Compute_Hamming_Weight --
	----------------------------

	function Compute_Hamming_Weight (Values : Word_Vector.Vector) return Natural is
		use Word_Vector;
		Differential_Cursor : Word_Vector.Cursor := Values.First;
		Weight : Natural := 0;
	begin
		loop
			exit when not Has_Element (Differential_Cursor);
			Weight := Weight + Compute_Hamming_Weight (Element (Differential_Cursor));
			Next(Differential_Cursor);
		end loop;

		return Weight;
	end Compute_Hamming_Weight;

	----------------------------
	-- Compute_Hamming_Weight --
	----------------------------

	function Compute_Hamming_Weight (Values : Word_Array) return Natural is
		Weight : Natural := 0;
	begin
		for I in Values'Range loop
			Weight := Weight + Compute_Hamming_Weight(Values(I));
		end loop;

		return Weight;
	end Compute_Hamming_Weight;
end DXPL.Types;