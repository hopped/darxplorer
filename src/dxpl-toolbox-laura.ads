----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of 'Lazy Laura'.
----------------------------------------------------------------
generic
	--  omitted
package DXPL.Toolbox.Laura is
	--  This package implements the behaviour expected by
	--  the technique 'Lazy Laura'.

	type Instance is new Toolbox.Object with private;

	------------------------------
	-- Functions and Procedures --
	------------------------------

	overriding
	procedure Analyse (Technique : in Instance; Current_State : in out Round_State);
	--  See DXPL.Toolbox for general information.

------------------
-- Private Part --
-------------------

private
	type Instance is new Toolbox.Object with null record;

end DXPL.Toolbox.Laura;
