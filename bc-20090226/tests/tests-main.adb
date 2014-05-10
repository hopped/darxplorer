--  $Id: tests-main.adb 1402 2009-02-22 11:48:45Z simonjwright $
--
--  Tests for the Booch Components.

with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Tests.Items;

with Tests.Auto_Pointers;
with Tests.AVL_Trees;
with Tests.Collections;
with Tests.Managed_Storage;
with Tests.Multiway_Trees;
with Tests.Rings;

procedure Tests.Main is

   function Suites return AUnit.Test_Suites.Access_Test_Suite;
   procedure Run is new AUnit.Run.Test_Runner (Suites);
   function Suites return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, Items.Suite);
      AUnit.Test_Suites.Add_Test (Result, Auto_Pointers.Suite);
      AUnit.Test_Suites.Add_Test (Result, AVL_Trees.Suite);
      AUnit.Test_Suites.Add_Test (Result, Collections.Suite);
      AUnit.Test_Suites.Add_Test (Result, Managed_Storage.Suite);
      AUnit.Test_Suites.Add_Test (Result, Multiway_Trees.Suite);
      AUnit.Test_Suites.Add_Test (Result, Rings.Suite);
      return Result;
   end Suites;

   Reporter : AUnit.Reporter.Text.Text_Reporter;

begin
   Run (Reporter);
end Tests.Main;
