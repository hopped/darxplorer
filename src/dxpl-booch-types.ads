with DXPL.Types;		  use DXPL.Types;
with DXPL.Support.State;  use DXPL.Support.State;
with BC.Graphs;
with BC.Graphs.Directed;
with BC.Graphs.Directed.Sorted;
with DXPL.Booch.Heap;

pragma Elaborate_All (BC.Graphs);
pragma Elaborate_All (BC.Graphs.Directed);
pragma Elaborate_All (BC.Graphs.Directed.Sorted);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  Package keeps track of some graph declarations used by 
--!  other packages.
----------------------------------------------------------------
package DXPL.Booch.Types is

    package Generic_Graph is new BC.Graphs
		(Vertex_Item => Round_State,
		 Arc_Item    => Conditioned_Float,
		 Storage     => DXPL.Booch.Heap.Storage);
    --  declared a convenient graph

    package Directed_Graph is new Generic_Graph.Directed;
    package Priority_Graph is new Directed_Graph.Sorted;
	--  We will use a priority driven graph to store
	--  our results from the analyzation of a hash function.

	Conditional_Arc : Directed_Graph.Arc;
	--  Probabilities will be stored in the edges of a graph.
end DXPL.Booch.Types;
