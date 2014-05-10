with DXPL_Types_32; use DXPL_Types_32;

------------------------------------------------------------------------
--!  Copyright 2009 Chair of Media Security / Prof. Dr. Stefan Lucks
--!
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/18/2009
--!  Last-Modified :: 09/18/2009
--!
--!  TEST
--!    * BEGIN_DECLARATION (POSITIVE)
--!
--!  Length of 'DXPL_MESSAGE' and 'DXPL_DIGEST' is not the same.
--!  Compiler should accept.
------------------------------------------------------------------------
procedure Begin_Decl_P_04 is

    --------------------
    --  DXPL_Process  --
    --------------------

    procedure DXPL_Process (Message : in out Word_Array_2) is
    begin
        null;
    end DXPL_Process;
    
-------------
--  SETUP  --
-------------
    
begin
    Configuration
        (DXPL_ALGORITHM   => "Simple Hash",
         DXPL_ROUNDS      => 10,
         DXPL_TERMINATION => 256);

    Test_Vector
        (DXPL_MESSAGE => (0 => 16#01234567#, 1 => 16#89abcdef#),
         DXPL_DIGEST  => (0 => 16#126c6b92#, 1 => 16#c0653a3e#,
                          2 => 16#126c6b92#, 3 => 16#c0653a3e#));

end Begin_Decl_P_04;
