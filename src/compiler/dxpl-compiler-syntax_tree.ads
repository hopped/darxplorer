with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with BC.Containers;
with BC.Containers.Trees;
with BC.Containers.Trees.Binary;
with BC.Containers.Trees.Binary.Post_Order;
with DXPL.Compiler.Symbol_Table;
with DXPL.Compiler.Support.State;
with Global_Heap;

pragma Elaborate_All (BC.Containers);
pragma Elaborate_All (BC.Containers.Trees.Binary);
pragma Elaborate_All (BC.Containers.Trees.Binary.Post_Order);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  Interface between the parsing process and the command line.
--!  A user can provide a valid description of a compression
--!  function by means of a filename.
----------------------------------------------------------------
generic
	S_Table : Symbol_Table.Instance;
	R_State : DXPL.Compiler.Support.State.Instance;
package DXPL.Compiler.Syntax_Tree is
	package ASU renames Ada.Strings.Unbounded;
	package Round_State renames DXPL.Compiler.Support.State;

	package Another_Container is new BC.Containers (Item => ASU.Unbounded_String, "=" => ASU."=");
	package Some_Trees        is new Another_Container.Trees;
	package Binary_Trees      is new Some_Trees.Binary (Storage => Global_Heap.Storage);

	package Tree_Vector is new Ada.Containers.Vectors
		(Element_Type => Binary_Trees.Binary_Tree,
	     Index_Type   => Positive,
		 "=" => Binary_Trees."=");

	type Sum_Type is
		record
			Sum          : ASU.Unbounded_String := ASU.Null_Unbounded_String;
			Left_Addend  : ASU.Unbounded_String := ASU.Null_Unbounded_String;
			Right_Addend : ASU.Unbounded_String := ASU.Null_Unbounded_String;
		end record;

	package Statement_Vector is new Ada.Containers.Vectors (Element_Type => Sum_Type, Index_Type => Positive);

	function Get_Infix_Statement
		(T : in Binary_Trees.Binary_Tree; Cut : in Boolean := False; Inline : in Boolean := False) return String;

	function  Split_Addition  (Item : in Binary_Trees.Binary_Tree) return Sum_Type;

	procedure Print_Tree      (T : Binary_Trees.Binary_Tree; Message : String := ""; Depth : Natural := 0);
	procedure Count_Addition  (T : in Binary_Trees.Binary_Tree; Count_Plus : in out Natural; Depth : in out Natural);
	procedure Filter_Addition (T : in Binary_Trees.Binary_Tree; Expression : in out Statement_Vector.Vector);

private
	procedure Process_Item (Item : ASU.Unbounded_String; Success : out Boolean);
	procedure Operand_Tree_Order is new Binary_Trees.Post_Order (Process_Item);

	package Operator_Vector is new Ada.Containers.Vectors 
		(Element_Type => ASU.Unbounded_String, Index_Type => Positive, "=" => ASU."=");
	Operator_Stack : Operator_Vector.Vector;

end DXPL.Compiler.Syntax_Tree;
