with Ada.Text_IO;
with DXPL.Support.State;  use DXPL.Support.State;
with DXPL.Types;          use DXPL.Types;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/17/2009
--!  Last-Modified :: 09/18/2009

--!  Purpose:
--!  Declaration of a convenient interface of formatting functions. It
--!  cannot be instantiated, but you are able to create child classes
--!  implementing this interface. The formatter is used in the logger
--!  to allow user-defined output. An default implementation can be
--!  found in 'DXPL.IO.Formatter.Default'.
--!
--!  While instantiating a toolbox, the formatter serves as a generic
--!  paramteter for the logger.
------------------------------------------------------------------------
package DXPL.IO.Formatter is

	type Object is abstract tagged private;

	type Level is (Stage, Round, Complete);

	function Convert (Obj   : in Object;
					  Item  : in Round_State;
					  HW    : in Natural; -- Hamming Weight as Natural
					  State : in Level) return String is abstract;

	function Convert (Obj : in Object; Item : in Conditioned_Float)
	  return String is abstract;

	function Convert (Obj : in Object; Item : in Word)
	  return String is abstract;

	function Convert (Obj : in Object; Item : in Word_Array)
	  return String is abstract;

private
	type Object is abstract tagged null record;
end DXPL.IO.Formatter;
