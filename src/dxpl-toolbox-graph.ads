with DXPL.Booch.Types;  use DXPL.Booch.Types;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of the analyzation with 'Greedy Grete'
--!  and 'Pedantic Petra', too.
----------------------------------------------------------------
generic
	-- omitted
package DXPL.Toolbox.Graph is
	--  This package provides implementations that can be
	--  used either with 'Greedy Grete' or 'Pedantic Petra'.

	type Instance is new Toolbox.Object with private;

	------------------------------
	-- Functions and Procedures --
	------------------------------

	overriding
	procedure Analyse (Technique : in Instance; Current_State : in out Round_State);

------------------
-- Private Part --
-------------------

private
	type Instance is new Toolbox.Object with null record;
	Forward_Graph : Priority_Graph.Graph_Instance;

	Round_Threshold : Rounds := Rounds'First;
	--  Traces the maximum round reached in processing.
end DXPL.Toolbox.Graph;