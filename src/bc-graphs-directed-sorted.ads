with Ada.Containers.Indefinite_Vectors;
with Ada.Unchecked_Conversion;
with DXPL.Types;  use DXPL.Types;
with DXPL.Booch.Priority_Queue;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  We implemented a child class of a graph representation 
--!  provided by the Ada Booch library to suit our own needs.
----------------------------------------------------------------
generic
package BC.Graphs.Directed.Sorted is

	type Graph_Instance is new Graph with null record;

	----------------------
	-- Graph operations --
	----------------------

	procedure Create_Connection (G    : in out Graph_Instance;
								 A    : in out Arc'Class;
								 I    : in     Arc_Item;
								 From : in out Vertex'Class;
								 To   : in out Vertex'Class);
	--  creates a new edge between the to given vertices "head" and "tail"
	--  in addition, the new item is added to the priority queue!

	function Dequeue_Next_Best_Node return Vertex'Class;
	--  dequeues the most probable vertex from the priority queue

	procedure Add_Last (I : in Arc_Item; Item : in out Vertex'Class);
	--  Adds a new item at the end of the priority queue. The last
	--  item of the queue will be removed, if the maximum size
	--  of the queue is reached.

	function Is_Queue_Empty return Boolean;
	--  returns true, if the priority queue is empty

	function Size_Of_Queue return Integer;
	--  returns the current size of the queue

	procedure Clear_Queue;
	--  resets the queue

	subtype Minimal_Index is Natural range 0 .. 1;
	package Vertex_Vector is new Ada.Containers.Indefinite_Vectors
	 (Element_Type => Vertex'Class, Index_Type => Minimal_Index);
	--  a vector keeping track of vertices

------------------
-- Private Part --
------------------

private

	type Sortable is
		record
			Key      : Vertex_Vector.Vector;
			Priority : Conditioned_Float;
		end record;

	function "<" (Left, Right : Sortable) return Boolean;

	package Sorted_Queue is new DXPL.Booch.Priority_Queue 
		(Element => Sortable, "<" => "<", Size => 100);
	PQueue : Sorted_Queue.QB.Queue;

	-----------------------------------
	-- Suitable unchecked conversion --
	-----------------------------------

	function Convert is new Ada.Unchecked_Conversion 
		(Source => Arc_Item, Target => Conditioned_Float);
end BC.Graphs.Directed.Sorted;
