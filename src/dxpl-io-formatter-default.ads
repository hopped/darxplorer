------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/17/2009
--!  Last-Modified :: 09/18/2009

--!  Purpose:
--!  Default implementation of the 'Formatter' interface. Incoming
--!  data types are converted as follows:
--!  (1) Conditioned_Float >> 1.2500E-01 [2^(-3)]
--!  (2) Word >> 0000_1000 including leading zeros and underscores.
--!  (3) Word_Array >> see (2) 
------------------------------------------------------------------------
package DXPL.IO.Formatter.Default is
	type Instance is new Formatter.Object with private;

	overriding
	function Convert (Obj   : in Instance;
					  Item  : in Round_State;
					  HW    : in Natural;  -- Hamming Weight
					  State : in Level) return String;
	--  CSV file formatting

	overriding
	function Convert (Obj : in Instance; Item : in Conditioned_Float) return String;

	overriding
	function Convert (Obj : in Instance; Item : in Word) return String;

	overriding
	function Convert (Obj : in Instance; Item : in Word_Array) return String;

private
	type Instance is new Formatter.Object with null record;

end DXPL.IO.Formatter.Default;
