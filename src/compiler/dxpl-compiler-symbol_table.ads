with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package DXPL.Compiler.Symbol_Table is
 
	type Instance is null record;

  ---------------------------------------------------------
  --  No_Element_Found
  --
  --  Purpose:
  --   Is thrown if no element is found...
  ---------------------------------------------------------
   No_Element_Found : EXCEPTION;


  type symbol_type is (t_function, t_variable, t_scope, t_procedure, t_parameter); 
  type symbol_state is (s_declared, s_implemented, s_none); 
  ---------------------------------------------------------
  --  symbol_entry
  --
  --  Purpose:
  --   Symbol_Type defines atomic entry in the symboltable
  ---------------------------------------------------------
  type symbol_entry is record
    s_name : Ada.Strings.Unbounded.Unbounded_String;
    s_type : symbol_type;
    s_state : symbol_state;
  end record;

  subtype Sym_Count is Positive range 1 .. 1_000;
  
  package symtable is new Ada.Containers.Vectors(
    Index_Type => Sym_Count,
    Element_Type => symbol_entry);

  ---------------------------------------------------------
  --  push
  --
  --  Purpose:
  --   This functions adds an entry to the symboltable
  ---------------------------------------------------------
  procedure push(object : instance;
	             s_name : in out Ada.Strings.Unbounded.Unbounded_String;
                 s_type : symbol_type;
                 s_state : symbol_state := s_declared);

  ---------------------------------------------------------
  --  has_element
  --
  --  Purpose:
  --   This function returns an elemnt or throws an exception
  ---------------------------------------------------------
  function has_element( object : instance; 
						s_name : Ada.Strings.Unbounded.Unbounded_String;
                         s_type : symbol_type;
       					Current_Scope_Only : Boolean := False) return Boolean;

  procedure begin_scope (object : instance);
  
  PROCEDURE End_Scope (object : instance);
  
   procedure print_stack (object : instance);

   
private
    
end DXPL.Compiler.Symbol_Table;
