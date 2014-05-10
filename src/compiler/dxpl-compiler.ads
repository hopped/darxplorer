with Ada.Exceptions;  use Ada.Exceptions;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  Declaration of a new package structure including comman
--!  expceptions used across the whole compiler framework.
----------------------------------------------------------------
package DXPL.Compiler is

	------------------
	--  Exceptions  --
	------------------

	Wrong_Element            : exception;
	No_More_Tokens           : exception;
	Wrong_Technique          : exception;
	Unsupported_Declaration  : exception;
	Not_Implemented          : exception;
	Loop_Out_of_Range        : exception;
    Loop_Exception           : exception;
    DXPL_Procedure_Exception : exception;
    Begin_Is_Missing         : exception;
    Configuration_Error      : exception;
    Configuration_Is_Missing : exception;
    No_Test_Vectors          : exception;
    Inconsistent_Vectors     : exception;


	type Input_Type is (MEMORY, FILE);
	--  Differentiate between input coming from memory or from file.

	type Error_Function is access procedure (Item : Exception_Id; Message : in String := "");
	Error_Access : Error_Function;

end DXPL.Compiler;
