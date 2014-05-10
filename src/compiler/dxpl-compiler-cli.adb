with Ada.Command_Line;
with Ada.Text_IO;
with DXPL.Compiler.Parser;
with Ada.Exceptions;  use Ada.Exceptions;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 08/19/2009
--!  Last-Modified :: 08/19/2009

--!  Purpose:
--!  Interface between the parsing process and the command line.
--!  A user can provide a valid description of a compression
--!  function by means of a filename.
----------------------------------------------------------------
procedure DXPL.Compiler.CLI is
	package Parser is new DXPL.Compiler.Parser;-- (Iteration_Threshold => 5);
begin
	if Ada.Command_Line.Argument_Count < 1 then
		Ada.Text_IO.Put_Line ("Please provide a valid file, which describes a compression function.");
		return;
	end if;

	declare
		Valid : Boolean := Parser.Parse (Ada.Command_Line.Argument (1));
	begin
		if Valid then
			Ada.Text_IO.Put ("TRUE");
		else
			Ada.Text_IO.Put ("FALSE");
		end if;
	end;
end DXPL.Compiler.CLI;
