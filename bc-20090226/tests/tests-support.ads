--  $Id: tests-support.ads 1385 2008-12-30 18:00:00Z simonjwright $
--
--  Non-simple item kinds for tests.

package Tests.Support is

   type Item is record
      C : Character;
      I : Integer;
   end record;

   function "=" (L, R : Item) return Boolean;
   --  Compares only the Character components (case-insensitively).

   function "<" (L, R : Item) return Boolean;
   --  Compares only the Character components (case-insensitively).

end Tests.Support;
