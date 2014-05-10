with DXPL.Types;  use DXPL.Types;
with DXPL.IO.Formatter.Default;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Interface definition for several techniques.
----------------------------------------------------------------
generic
	Toolbox_Threshold : Conditioned_Float;
	pragma Warnings (Off, Toolbox_Threshold);
package DXPL.Technique is
--  Each of the techniques should implement a procedure with the
--  following parameter list:
--
--  X, Y : in Word;
--  Z    : in out Word;
--  Output_Candidates : in out Word_Vector.Vector;
--  Probability       : in out Conditioned_Float;
--
--  to support the integration of a particular technique
--  in the framework. The procedure serves as a parameter
--  the the generic round founction declared in DXPL.Round_Function.

	Default_Formatter : DXPL.IO.Formatter.Default.Instance;
	--  Default formatter, that can be used for the logger
	--  by all techniques.
end DXPL.Technique;