with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;
with Ada.Text_IO;

--  Implementation Notes:
--
--  Anticipated Changes:
--
--------------------------------------------------------------
PACKAGE BODY DXPL.Compiler.Symbol_Table IS
	use Ada.Strings.Unbounded;
	
   V : Symtable.Vector;
   --------------------------------------------------------------
   procedure Push (object : instance;
         S_Name      : in out Ada.Strings.Unbounded.Unbounded_String;
         S_Type      : Symbol_Type;
         S_State     : Symbol_State                           := S_Declared)
     IS
      Lower_Name : Ada.Strings.Unbounded.Unbounded_String := S_Name;
      New_Entry : Symbol_Entry;

   BEGIN -- push
	ada.strings.unbounded.replace_element (s_name, 1, ada.strings.unbounded.element (s_name, 1));--Ada.Characters.Handling.To_Lower (ada.strings.unbounded.element (s_name, 1)));

	Lower_Name := Ada.Strings.Unbounded.Translate(S_Name, Ada.Strings.Maps.Constants.Lower_Case_Map);
	
      -- WENN SCHON fwd decl stattgefunden hat!!!
      IF S_Type = T_Function AND S_State = S_Implemented AND Has_Element(Object, Lower_Name, S_Type, False)
            THEN
         DECLARE
            Current_Element : Symbol_Entry;
         BEGIN
            FOR I IN 1 .. Symtable.Length(V) LOOP
               Current_Element := Symtable.Element(V, Positive(I));

                        IF Current_Element.S_Name = Lower_Name AND Current_Element.S_Type = S_Type AND Current_Element.S_State = S_declared THEN
                  Current_Element.S_State := S_Implemented;
                  Symtable.Replace_Element(v, Positive(I), Current_Element);
                 -- RETURN(True);
               END IF;
            END LOOP;
            --Return False;
         END;
      END IF;

      --IF not Has_Element(S_Name, S_Type, S_Type /= T_Function) THEN
         New_Entry := (
            S_Name      => Lower_Name,
            S_Type      => S_Type,
            S_State     => S_State);

         Symtable.Append(V, New_Entry);

      --END IF;
	
--	ada.text_io.put_line ("push " & ada.strings.unbounded.to_string(s_name));

      Print_Stack (Object);

      --RETURN True;
   END Push;

   --------------------------------------------------------------
   FUNCTION Has_Element (
         object : instance;
         S_Name : Ada.Strings.Unbounded.Unbounded_String;
         S_Type : Symbol_Type;
         Current_Scope_Only : Boolean := False) RETURN Boolean IS
     pragma Warnings (Off, Object);
     Current_Element : Symbol_Entry;
     Lower_Name : Ada.Strings.Unbounded.Unbounded_String := Ada.Strings.Unbounded.Translate(S_Name, Ada.Strings.Maps.Constants.Lower_Case_Map);
   BEGIN -- has_element
      IF NOT Symtable.Is_Empty(V) THEN
         FOR I IN reverse 1 .. Symtable.Length(V) LOOP
            Current_Element := Symtable.Element(V, Positive(I));

            IF Current_Scope_Only AND Current_Element.S_Type = T_Scope THEN
               EXIT;
            END IF;
            
               IF Current_Element.S_Name = Lower_Name AND Current_Element.S_Type = S_Type THEN

                  RETURN True;
               END IF;
            
         END LOOP;
      END IF;

      RETURN False;
   END Has_Element;

 


   --------------------------------------------------------------
   PROCEDURE Begin_Scope (object : instance) IS
	pragma Warnings (Off, Object);
      New_Entry : Symbol_Entry;

   BEGIN -- begin_scope
      
      New_Entry := (
         S_Name      => To_Unbounded_String ("new scope"),
         S_Type      => T_Scope,
         S_State     => S_None);

      Symtable.Append(V, New_Entry);
   END Begin_Scope;

   --------------------------------------------------------------
   PROCEDURE End_Scope (object : instance) IS
		pragma Warnings (Off, Object);
      Current_Element : Symbol_Entry;
   BEGIN -- end_scope
      
      IF NOT Symtable.Is_Empty(V) THEN
         FOR I IN REVERSE 1 .. Symtable.Length(V) LOOP
            Current_Element := Symtable.Element(V, Positive(I));
            IF NOT (Current_Element.S_Name = To_Unbounded_String("new scope") AND Current_Element.S_Type = T_Scope) THEN
               Symtable.Delete_Last(V);
            ELSE
               Symtable.Delete_Last(V);
               EXIT;
            END IF;
         END LOOP;
      END IF;
   END End_Scope;


   PROCEDURE Print_Stack (object : instance) IS
		pragma Warnings (Off, Object);
   BEGIN -- print stack
   null;
   END Print_Stack;


END DXPL.Compiler.Symbol_Table;
