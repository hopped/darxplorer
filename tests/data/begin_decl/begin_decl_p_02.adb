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
--!  More than one DXPL_Types.Test_Vector declaration are allowed.
--!  Compiler should accept.
------------------------------------------------------------------------
procedure Begin_Decl_P_02 is

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
    DXPL_Types_32.Configuration
        (DXPL_ALGORITHM   => "Simple Hash",
         DXPL_ROUNDS      => 10,
         DXPL_TERMINATION => 256);

    DXPL_Types_32.Test_Vector
        (DXPL_MESSAGE => (0 => 16#01234567#, 1 => 16#89abcdef#),
         DXPL_DIGEST  => (0 => 16#126c6b92#, 1 => 16#c0653a3e#));

    DXPL_Types_32.Test_Vector
        (DXPL_MESSAGE => (0 => 16#64636261#, 1 => 16#00000000#),
         DXPL_DIGEST  => (0 => 16#12345678#, 1 => 16#87654321#));

end Begin_Decl_P_02;
