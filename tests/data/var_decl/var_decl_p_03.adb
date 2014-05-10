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
--!  Simple variable declaration with default initialisation.
--!  Compiler hat to accept.
------------------------------------------------------------------------
procedure Var_Decl_P_03 is

    ------------------------------
    --  Variables declarations  --
    ------------------------------

   IV : DXPL_Types_64.Word_Array_8 :=
     (16#8081828384858687#,
      16#88898a8b8c8d8e8f#,
      16#9091929394959697#,
      16#98999a9b9c9d9e9f#,
      16#a0a1a2a3a4a5a6a7#,
      16#a8a9aaabacadaeaf#,
      16#b0b1b2b3b4b5b6b7#,
      16#b8b9babbbcbdbebf#);

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

end Var_Decl_P_03;

