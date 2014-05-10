--  Stefan Lucks 2003/12/11
--
--  Debug:
--  A simple package for program tracing and assertions (if in debug mode).
--  If a program that with's this package, has a "-d"/"-D" command line arg,
--  debug mode is on, else debug mode is off.  User code can change mode.
--  It is based on a package from Ehud Lamm.
package DXPL.Compiler.Debug is

   procedure Assert (Condition : Boolean; Error_Message : String := "***");
   --  in debug mode: if not Condition
   --                  then write Error_Message and raise Assert_Failed
   --  not in debug mode: do nothing.

   procedure Trace (Message : String);
   --  in debug mode: write Message
   --  not in debug mode: do nothing

   procedure Mode (On : Boolean);
   function Mode return Boolean;
   --  write and read debug mode

   Assert_Failed : exception;

end DXPL.Compiler.Debug;
