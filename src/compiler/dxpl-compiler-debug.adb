with Ada.Text_IO;
with Ada.Command_Line;

package body DXPL.Compiler.Debug is

   package CL renames Ada.Command_Line;

   Mode_On : Boolean := False;
   --  Assert_Failed: exception;

   procedure Assert (Condition : Boolean; Error_Message : String := "***") is
   begin
      if Mode_On and (Condition = False) then
         Trace ("*** Assert Failed***");
         Trace (Error_Message);
         raise Assert_Failed;
      end if;
   end Assert;
   pragma Inline (Assert);

   procedure Trace (Message : String) is
	 Former_File : Ada.Text_IO.File_Type := Ada.Text_IO.Current_Output;
   begin
	Ada.Text_IO.Set_Output (File => Ada.Text_IO.Standard_Output);
      if Mode_On then
         Ada.Text_IO.Put_Line (Message);
      end if;
	Ada.Text_IO.Set_Output (File => Former_File);
   end Trace;
   pragma Inline (Trace);

   procedure Mode (On : Boolean) is
   begin
      Mode_On := On;
   end Mode;

   function Mode return Boolean is
   begin
      return Mode_On;
   end Mode;

begin
   for i in 1 .. CL.Argument_Count loop
      declare
         Arg : String := CL.Argument (i);
      begin
         if Arg'Length = 2 then
            if (Arg = "-d") or (Arg = "-D") then
               Mode (True);
            end if;
         end if;
      end;
   end loop;
end DXPL.Compiler.Debug;
