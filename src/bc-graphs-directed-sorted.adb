package body BC.Graphs.Directed.Sorted is
	use Sorted_Queue; 
	--  PQueue now available

	-----------------
	-- Clear_Queue --
	-----------------

	procedure Clear_Queue is
	begin
		PQueue.Clear;
	end Clear_Queue;
	pragma Inline (Clear_Queue);

	---------
	-- "<" --
	---------

	function "<" (Left, Right : Sortable) return Boolean is
	begin
		return Left.Priority > Right.Priority;
	end "<";

	------------
	-- Append --
	------------

	procedure Append (P : in Sortable) is
	begin
		PQueue.Append (P);
	end Append;
	pragma Inline (Append);

	----------------------
	-- Graph operations --
	----------------------

	-----------------------
	-- Create_Connection --
	-----------------------

	procedure Create_Connection
		(G    : in out Graph_Instance;
		 A    : in out Arc'Class;
		 I    : in     Arc_Item;
		 From : in out Vertex'Class;
		 To   : in out Vertex'Class)
	is
		V : Vertex_Vector.Vector;
		P : Sortable;
	begin
		Create_Arc (G, A, I, To, From);
		Vertex_Vector.Append (V, To);
		P := (V, Convert (I));

		Append (P);
	end Create_Connection;

	procedure Add_Last (I : in Arc_Item; Item : in out Vertex'Class) is
		V : Vertex_Vector.Vector;
		P : Sortable;
	begin
		Vertex_Vector.Append (V, Item);
		P := (V, Convert (I));
		PQueue.Insert (P);
	end Add_Last;

	----------------------------
	-- Dequeue_Next_Best_Node --
	----------------------------

	function Dequeue_Next_Best_Node return  Vertex'Class is
		P : Sortable := PQueue.Front;
		V : Vertex_Vector.Vector;
	begin
		V := P.Key;
		PQueue.Pop;

		return Vertex_Vector.Element (V, Minimal_Index'First);
	end Dequeue_Next_Best_Node;

	--------------------
	-- Is_Queue_Empty --
	--------------------

	function Is_Queue_Empty return Boolean is
	begin
		return PQueue.Is_Empty;
	end Is_Queue_Empty;
	pragma Inline (Is_Queue_Empty);

	-------------------
	-- Size_Of_Queue --
	-------------------

	function Size_Of_Queue return Integer is
	begin
		return PQueue.Length;
	end Size_Of_Queue;
	pragma Inline (Size_Of_Queue);
end BC.Graphs.Directed.Sorted;