------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/20/2009
--!  Last-Modified :: 09/20/2009

--!  Purpose:
--!  Providing tests for the branch
--!    * func_decl

--!  File was tested with tg-3.1
--!  See <http://www.free-software-consulting.com/projects/tg/>
------------------------------------------------------------------------
ERROR_HANDLING  stop

CONTEXT with Ada.Text_IO;  use Ada.Text_IO;
        with DXPL.Compiler.Parser;
        with DXPL.Compiler.Support.Annotation;

DEFINE  package Parser is new DXPL.Compiler.Parser;
        package Annotation renames DXPL.Compiler.Support.Annotation;
        Directory : String := "./tests/data/func_decl/";
        Valid  : Boolean := False;

-------------------------
--  POSITIVE EXAMPLES  --
-------------------------

*****   func_decl_p_00.adb
TEST    Valid := Parser.Parse (Directory & "func_decl_p_00.adb");
PASS    Valid = True

*****   func_decl_p_01.adb
TEST    Valid := Parser.Parse (Directory & "func_decl_p_01.adb");
PASS    Valid = True
