with DXPL_Types_32; use DXPL_Types_32;

------------------------------------------------------------------------
--!  Copyright 2009 Chair of Media Security / Prof. Dr. Stefan Lucks
--!
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/18/2009
--!  Last-Modified :: 09/18/2009
--!
--!  TEST
--!    * BEGIN_DECLARATION (NEGATIVE)
--!
--!  DXPL_Types.Configuration is missing. Compiler should mumble.
------------------------------------------------------------------------
procedure Begin_Decl_N_00 is

    --------------------
    --  DXPL_Process  --
    --------------------

    procedure DXPL_Process (Message : in out DXPL_Types_32.Word_Array_2) is
    begin
        null;
    end DXPL_Process;
    
-------------
--  SETUP  --
-------------
    
begin
    DXPL_Types_32.Test_Vector
        (DXPL_MESSAGE => (0 => 16#01234567#, 1 => 16#89abcdef#),
         DXPL_KEY     => (0 => 16#00112233#, 1 => 16#44556677#,
                          2 => 16#8899aabb#, 3 => 16#ccddeeff#),
         DXPL_DIGEST  => (0 => 16#126c6b92#, 1 => 16#c0653a3e#));

end Begin_Decl_N_00;
