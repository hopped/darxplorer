with BC.Containers;
with BC.Containers.Queues;
with BC.Containers.Queues.Ordered;
with BC.Containers.Queues.Ordered.Bounded;

pragma Elaborate_All (BC.Containers);
pragma Elaborate_All (BC.Containers.Queues);
pragma Elaborate_All (BC.Containers.Queues.Ordered);
pragma Elaborate_All (BC.Containers.Queues.Ordered.Bounded);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  Instantiation of a priority queue limited to a maximum size.
----------------------------------------------------------------
generic
	type Element is private;
	with function "<" (L, R : Element) return Boolean is <>;
	Size : Integer := 20;
package DXPL.Booch.Priority_Queue is
	package Containers  is new BC.Containers  (Item => Element);
	package Base_Queues is new Containers.Queues;
	package Queues      is new Base_Queues.Ordered;
	package QB          is new Queues.Bounded (Maximum_Size => Size);
end DXPL.Booch.Priority_Queue;