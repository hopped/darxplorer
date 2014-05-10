--  $Id: tests-support.adb 1385 2008-12-30 18:00:00Z simonjwright $
--
--  Non-simple item kinds for tests.

with Ada.Strings.Maps.Constants;

package body Tests.Support is


   use Ada.Strings.Maps;
   use Ada.Strings.Maps.Constants;


   --  Compares only the Character components (case-insensitively).
   function "=" (L, R : Item) return Boolean is
   begin
      return Value (Lower_Case_Map, L.C) = Value (Lower_Case_Map, R.C);
   end "=";


   --  Compares only the Character components (case-insensitively).
   function "<" (L, R : Item) return Boolean is
   begin
      return Value (Lower_Case_Map, L.C) < Value (Lower_Case_Map, R.C);
   end "<";


end Tests.Support;
