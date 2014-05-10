----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  This subunit is declared to minimize the effort to inte-
--!  grate DXPL.Round_Function into the framework. The function
--!  Get_Key is completly static and need no changes at all.
----------------------------------------------------------------
separate (DXPL.Round_Function)

function Get_Key (Index : in Positive) return Word_Array is
begin
	return Test_Vectors.Batch (Index).Key.all;
end Get_Key;