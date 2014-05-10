----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  This subunit represents a null procedure for the
--!  initialization step. If a user does not declare explicitly
--!  an Initialize procedure in his description of a hash func-
--!  tion, this dummy will be called to maintain the generic
--!  behaviour of the framework.
----------------------------------------------------------------
separate (DXPL.Round_Function)

procedure Initialize (State : access Round_State) is
begin
	null;
end Initialize;