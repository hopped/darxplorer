with Ada.Strings.Fixed;

package body DXPL.IO is

	----------------------------------
	-- Get_Hex_Char_From_Bin_String --
	----------------------------------

	function Get_Hex_Char_From_Bin_String (Item : in String) return Character is
	begin
		if Item = "0000" then return '0'; end if;
		if Item = "0001" then return '1'; end if;
		if Item = "0010" then return '2'; end if;
		if Item = "0011" then return '3'; end if;
		if Item = "0100" then return '4'; end if;
		if Item = "0101" then return '5'; end if;
		if Item = "0110" then return '6'; end if;
		if Item = "0111" then return '7'; end if;
		if Item = "1000" then return '8'; end if;
		if Item = "1001" then return '9'; end if;
		if Item = "1010" then return 'A'; end if;
		if Item = "1011" then return 'B'; end if;
		if Item = "1100" then return 'C'; end if;
		if Item = "1101" then return 'D'; end if;
		if Item = "1110" then return 'E'; end if;
		if Item = "1111" then return 'F'; end if;

		raise Program_Error;
	end Get_Hex_Char_From_Bin_String;

	--------------
	-- Show_Bin --
	--------------

	function Show_Bin (Item : in Word) return String is
		Bin_String : String (1 .. Word'Size);
		Length     : Positive := Word'Size + 1;
	begin
		for i in Bin_String'Range loop
			if (Item and (0 or 2**(i - 1))) = 2**(i - 1) then
				Bin_String (Length - i) := '1';
			else
				Bin_String (Length - i) := '0';
			end if;
		end loop;

		return Bin_String;
	end Show_Bin;

	--------------
	-- Show_Hex --
	--------------

	function Show_Hex (Item : in Word) return String is
		Bin_String : String := Show_Bin(Item);
		Length : Positive := Word'Size/4;
		Hex_String : String (1 .. (Length + Length/4));
		--  Length/4 represents underscores
		Index : Natural := 2;
	begin
		--we convert 4 chars of the Bin_String to one Char Hex_String
		for I in 1 .. Length loop
			Hex_String (Index - 1) := Get_Hex_Char_From_Bin_String (Bin_String (4*I - 3 .. 4*I));
				if Index mod 5 = 0 then
					Hex_String (Index) := '_';
					Index := Index + 1;
				end if;
			Index := Index + 1;
		end loop;

		--  cuts off the last underscore
		return Hex_String (1 .. (Length + Length/4 - 1));
	end Show_Hex;

	-------------------------------------
	-- Convert_Float_To_Decimal_String --
	-------------------------------------

	function Convert_Float_To_Decimal_String (Probability : in Conditioned_Float) return String is
		use Ada.Strings.Fixed;
		N : Integer 		  := 0;
		T : Conditioned_Float := Probability;
	begin
		loop
			exit when Probability >= 2.0 ** (-N);
			T := T / 2.0;
			N := Integer'Succ(N);
		end loop;
	 	return "2^(-" & Trim (Integer'Image (N), Ada.Strings.Left) & ")";
	end Convert_Float_To_Decimal_String;
end DXPL.IO;
