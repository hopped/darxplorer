with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;
with DXPL.Types;          use DXPL.Types;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/08/2009
--!  Last-Modified :: 09/18/2009

--!  Purpose:
--!  Declaration of a new package structure.
------------------------------------------------------------------------
package DXPL.IO is

	-------------------------------
	-- Functions and Procedures --
	-------------------------------

	function Show_Bin (Item : in Word) return String;
	--  Returns a binary representation of the given Word

	function Show_Hex (Item : in Word) return String;
	--  Returns a hexadecimal representation of the given Word
	--  Each group of four digits is separated by an underscore

	function Convert_Float_To_Decimal_String
	  (Probability : in Conditioned_Float) return String;
	--  This procedure is called by Show_Round and Show_Stage. It provides
	--  a convenient method to convert the given probability to the base 2
	--  into a decimal format.

end DXPL.IO;
