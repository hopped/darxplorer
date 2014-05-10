--  $Id: tests-items.adb 1402 2009-02-22 11:48:45Z simonjwright $
--
--  Tests for Items.

with AUnit.Assertions; use AUnit.Assertions;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Text_IO; use Ada.Text_IO;

pragma Warnings (Off, Ada.Text_IO);
--  May not be referenced for released versions

with Tests.Support;

package body Tests.Items is


   subtype Item is Support.Item;
   use type Support.Item;


   type Case_1 is new Test_Case with null record;
   function Name (C : Case_1) return AUnit.Message_String;
   procedure Register_Tests (C : in out Case_1);


   -----------------------
   --  Test procedures  --
   -----------------------

   procedure Equality (C : in out Test_Case'Class);
   procedure Equality (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
   begin
      Assert (Item'('a', 0) = Item'('a', 0),
              "wholly equal items not equal");
      Assert (Item'('a', 0) = Item'('a', 1),
              "key-equal items not equal");
      Assert (Item'('a', 0) = Item'('A', 0),
              "case-insensitively equal items not equal - l,u");
      Assert (Item'('A', 0) = Item'('a', 0),
              "case-insensitively equal items not equal - u,l");
      Assert (Item'('a', 0) = Item'('A', 1),
              "case-insensitively key-equal items not equal - l,u");
      Assert (Item'('A', 0) = Item'('a', 1),
              "case-insensitively key-equal items not equal - u,l");
      Assert (not (Item'('a', 0) = Item'('b', 0)),
              "key-unequal items equal - a,b");
      Assert (not (Item'('b', 0) = Item'('a', 0)),
              "key-unequal items equal - b,a");
      Assert (not (Item'('A', 0) = Item'('B', 0)),
              "key-unequal items equal - A,B");
      Assert (not (Item'('B', 0) = Item'('A', 0)),
              "key-unequal items equal - B,A");
   end Equality;


   procedure Comparison (C : in out Test_Case'Class);
   procedure Comparison (C : in out Test_Case'Class) is
      pragma Warnings (Off, C);
   begin
      Assert (not (Item'('a', 0) < Item'('a', 0)),
              "wholly equal items less");
      Assert (not (Item'('a', 0) < Item'('A', 0)),
              "case-insensitively equal items less - l,u");
      Assert (not (Item'('A', 0) < Item'('a', 0)),
              "case-insensitively equal items less - u,l");
      Assert (not (Item'('a', 0) < Item'('a', 1)),
              "key-equal items less");
      Assert (not (Item'('a', 0) < Item'('A', 1)),
              "case-insensitively key-equal items less - l,u");
      Assert (not (Item'('A', 0) < Item'('a', 1)),
              "case-insensitively key-equal items less - u,l");
      Assert (Item'('a', 0) < Item'('b', 0),
              "wholly less items not less");
      Assert (Item'('a', 0) < Item'('B', 0),
              "case-insensitively less items not less - l,u");
      Assert (Item'('A', 0) < Item'('b', 0),
              "case-insensitively less items not less - u,l");
      Assert (Item'('a', 0) < Item'('b', 1),
              "key-less items not less");
      Assert (Item'('a', 0) < Item'('B', 1),
              "case-insensitively key-less items not less - l,u");
      Assert (Item'('A', 0) < Item'('b', 1),
              "case-insensitively key-less items not less - u,l");
   end Comparison;


   function Name (C : Case_1) return AUnit.Message_String is
      pragma Warnings (Off, C);
   begin
      return new String'("Items");
   end Name;


   procedure Register_Tests (C : in out Case_1) is
   begin
      Registration.Register_Routine
        (C,
         Equality'Unrestricted_Access,
         "Equality");
      Registration.Register_Routine
        (C,
         Comparison'Unrestricted_Access,
         "Comparison");
   end Register_Tests;


   function Suite return AUnit.Test_Suites.Access_Test_Suite is
      Result : constant AUnit.Test_Suites.Access_Test_Suite
        := new AUnit.Test_Suites.Test_Suite;
   begin
      AUnit.Test_Suites.Add_Test (Result, new Case_1);
      return Result;
   end Suite;


end Tests.Items;
