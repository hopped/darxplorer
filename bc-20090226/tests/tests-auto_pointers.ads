--  $Id: tests-auto_pointers.ads 1385 2008-12-30 18:00:00Z simonjwright $
--
--  Tests for Auto_Pointers.

with AUnit.Test_Suites;

package Tests.Auto_Pointers is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

end Tests.Auto_Pointers;
