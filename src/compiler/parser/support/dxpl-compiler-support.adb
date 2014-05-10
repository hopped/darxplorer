with Ada.Directories;

package body DXPL.Compiler.Support is

	procedure Dummy is
	begin
		null;
	end Dummy;

begin
	if not Ada.Directories.Exists (Directory) then
		Ada.Directories.Create_Directory (Directory);
	end if;
end DXPL.Compiler.Support;