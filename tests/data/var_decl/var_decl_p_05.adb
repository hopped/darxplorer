with DXPL_Types_32; use DXPL_Types_32;

------------------------------------------------------------------------
--!  Copyright 2009 Chair of Media Security / Prof. Dr. Stefan Lucks
--!
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/18/2009
--!  Last-Modified :: 09/18/2009
--!
--!  TEST
--!    * VARIABLES_DECLARATION (POSITIVE)
--!
--!  Simple array declaration. Compiler hat to accept.
------------------------------------------------------------------------
procedure Var_Decl_P_05 is

    ------------------------------
    --  Variables declarations  --
    ------------------------------

    type Constants is array (0 .. 3) of Integer;
    type Const_Array is array (0 .. 3) of Constants;
    S : Const_Array := 
     (0 => (1, 3, 4, 37),
      1 => (1, 2, 13, 43),
      2 => (2, 1, 19, 53),
      3 => (2, 2, 28, 59));

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

end Var_Decl_P_05;

