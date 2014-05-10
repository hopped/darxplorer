with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

-------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 08/06/2009
--!  Last-Modified :: 08/06/2009

--!  Purpose:
--!  This package adds support to associate parameter declarations
--!  of procedures with the corresponding arguments. It is useful
--!  to handle procedure calls with several arguments.
-------------------------------------------------------------------
package DXPL.Compiler.Support.Parameter is
	package ASU renames Ada.Strings.Unbounded;

	type Handover is (In_Value, Out_Value);
	--  Differentiates between arguments passed with 'in' or 'out'.
	--  If the type handover is set to 'Out_Value', the declaration
	--  of 'in' is implicit.

	procedure Add_Parameter 
		(Procedure_Name : in ASU.Unbounded_String;
		 Parameter_Name : in ASU.Unbounded_String;
		 Handover_Value : in Handover := In_Value);
	--  Adds a parameter, read from a procedure parameter declaration,
	--  to the internal state of parameters. The parameter is associated
	--  with its procedure. If a parameter is already added, no exception
	--  is raised.

	procedure Add_Argument (Item : in ASU.Unbounded_String);
	--  Adds an argument of a procedure call to an internal list

	procedure Reset_Arguments;
	--  Arguments should be reset to zero after processing them
	--  by means of 'Match_Handover' and 'Match_Return'

	package String_Vector is new Ada.Containers.Vectors
		(Element_Type => ASU.Unbounded_String, Index_Type => Positive, "=" => ASU."=");
	--  A vector holding unbounded strings

	function Match_Handover (Procedure_Name : in ASU.Unbounded_String) return String;
	--  Associates arguments with parameters and return a consecutive string
	--  containing expressions for each argument i like 'parameter_i := argument_i;'

	function Match_Return (Procedure_Name : in ASU.Unbounded_String) return String;
	--  Associates arguments with parameters and return a consecutive string
	--  containing expressions for each argument i like 'argument_i := parameter_i;'

	procedure Clear;
	--  Resets the internal state

private
	type Parameter_Element is
		--  Represents a simple pair of an variable identifier and 
		--  its handover type (in, out)
		record
			Value          : ASU.Unbounded_String;
			Parameter_Type : Handover := In_Value;
		end record;

	package Parameter_Vector is new Ada.Containers.Vectors
		(Element_Type => Parameter_Element, Index_Type => Positive, "=" => "=");
	--  A vector holding parameter elements

	function Parameter_Hash (Element : Parameter_Element) return Ada.Containers.Hash_Type;
	--  Computes a hash value for parameter elements by means of its value

	function Equivalent_Parameter (Left, Right : Parameter_Element) return Boolean;
	-- Defines the equality of two parameter elements

	function "=" (Left, Right : in Parameter_Element) return Boolean;
	--  Equality of parameter elements has to be defined to 
	--  declare a parameter set.

	package Parameter_Set is new Ada.Containers.Hashed_Sets
		(Element_Type => Parameter_Element,
		 Hash         => Parameter_Hash,
		 Equivalent_Elements => Equivalent_Parameter,
		 "="          => "=");
	--  A set of parameter elements

	package Parameter_Map is new Ada.Containers.Hashed_Maps
		(Key_Type     => ASU.Unbounded_String,
		 Element_Type => Parameter_Set.Set,
		 Hash         => ASU.Hash,
		 Equivalent_Keys => ASU."=",
		 "="          => Parameter_Set."=");
	--  A map of pairs (unbounded_string, parameter_set.set)

	Parameters : Parameter_Map.Map;
	--  Keeps track of parameters

	Arguments  : String_Vector.Vector;
	--  Keeps track of arguments

end DXPL.Compiler.Support.Parameter;
