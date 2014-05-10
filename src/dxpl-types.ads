with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/24/2009
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Provides common data types for all packages defined
--!  in the darXplorer framework. In addition, some useful
--!  operations like computing the hamming weight are given.
----------------------------------------------------------------
package DXPL.Types is
	type    Word_Array    is array (Integer range <>) of aliased Word;
	subtype Word_Array_80 is Word_Array (0 .. 79);
	subtype Word_Array_64 is Word_Array (0 .. 63);
	subtype Word_Array_32 is Word_Array (0 .. 31);
	subtype Word_Array_16 is Word_Array (0 .. 15);
	subtype Word_Array_8  is Word_Array (0 ..  7);
	subtype Word_Array_4  is Word_Array (0 ..  3);
	subtype Word_Array_3  is Word_Array (0 ..  2);
	subtype Word_Array_2  is Word_Array (0 ..  1);

	subtype Modified_Value is Integer range Word_Array_64'First .. Word_Array_64'Last;

	type Conditioned_Float is new Long_Long_Float range 0.0 .. 1.0;
	--  limits the float type to a more sensible co-domain

	package Word_Vector is new Ada.Containers.Vectors
		(Element_Type => Word, Index_Type => Natural);

	package Word_Set is new Ada.Containers.Ordered_Sets
		(Element_Type => Word);

	use Word_Set;
	package Vector_Of_Sets is new Ada.Containers.Vectors
		(Element_Type => Word_Set.Set, Index_Type => Natural);

	type Bit is new Natural range 0 .. 1;
	--  declares a single bit

	type Bit_Number is new Natural range 0 .. Word'Size - 1;
	--  represents a bit string in Ada of generic length

	type Bit_Field is array (Bit_Number) of Boolean;
	--  bit string representation as an array of booleans
	--  allows direct access to individual bits

	for Bit_Field'Component_Size use 1;
	--  replaces pragma pack(Bit_Field), because pack does
	--  not ensure to store each component of the array in 1 bit
		
	function To_Bit_Field is
		new Ada.Unchecked_Conversion (Source => Word, Target => Bit_Field);
	--  use unchecked_conversions to convert a modular type like 2**n into
	--  an array of boolean of length n to provide direct access to bits.
	--  another way of providing direct bit access can be realized by means of
	--  logical 'or' or logical 'and'. see Set_Bit and Get_Bit for more information.

	function To_Word is
		new Ada.Unchecked_Conversion (Source => Bit_Field, Target => Word);
	--  unchecked_conversion from a bit field represented by an array
	--  of booleans to a modular type. can be used to convert an existing
	--  array of booleans into a modular type.
			
	type Hamming_Weight is (HW_1, HW_2, HW_3, HW_4);

	function Compute_Hamming_Weight (Value : in Word) return Natural;
	--  see implementation for the concrete formula. used to compute the
	--  differential probability. formula is derived from the Wikipedia
	--  article 'Compute_Hamming_Weight', <http://en.wikipedia.org/wiki/Hamming_weight>
	--# pre : implementation based on 64 bit, so input values have to be of
	--        equal length or lower!

	function Compute_Hamming_Weight (Values : Word_Vector.Vector) return Natural;
	--  see implementation for the concrete formula. used to compute the
	--  differential probability. formula is derived from the Wikipedia
	--  article 'Compute_Hamming_Weight', <http://en.wikipedia.org/wiki/Hamming_weight>
	--# pre : implementation based on 64 bit, so input values have to be of
	--        equal length or lower!

	function Compute_Hamming_Weight (Values : Word_Array) return Natural;
	--  see implementation for the concrete formula. used to compute the
	--  differential probability. formula is derived from the Wikipedia
	--  article 'Compute_Hamming_Weight', <http://en.wikipedia.org/wiki/Hamming_weight>
	--# pre : implementation based on 64 bit, so input values have to be of
	--        equal length or lower!
end DXPL.Types;
