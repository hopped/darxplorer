------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/20/2009
--!  Last-Modified :: 09/20/2009

--!  Purpose:
--!  Providing tests for the branch
--!    * expression

--!  File was tested with tg-3.1
--!  See <http://www.free-software-consulting.com/projects/tg/>
------------------------------------------------------------------------
ERROR_HANDLING  stop

CONTEXT with Ada.Text_IO;  use Ada.Text_IO;
        with DXPL.Compiler.Parser;
        with DXPL.Compiler.Support.Annotation;

DEFINE  package Parser is new DXPL.Compiler.Parser;
        package Annotation renames DXPL.Compiler.Support.Annotation;
        Directory : String := "./tests/data/expression/";
        Valid  : Boolean := False;

-------------------------
--  NEGATIVE EXAMPLES  --
-------------------------

*****   expression_n_00.adb
TEST    Valid := Parser.Parse (Directory & "expression_n_00.adb");
PASS    exception DXPL.Compiler.Not_Implemented

*****   expression_n_01.adb
TEST    Valid := Parser.Parse (Directory & "expression_n_01.adb");
PASS    exception DXPL.Compiler.Not_Implemented

*****   expression_n_02.adb
TEST    Valid := Parser.Parse (Directory & "expression_n_02.adb");
PASS    exception DXPL.Compiler.Loop_Exception

*****   expression_n_03.adb
TEST    Valid := Parser.Parse (Directory & "expression_n_03.adb");
PASS    exception DXPL.Compiler.Loop_Exception

*****   expression_n_04.adb
TEST    Valid := Parser.Parse (Directory & "expression_n_04.adb");
PASS    exception DXPL.Compiler.Loop_Exception

*****   expression_n_05.adb
TEST    Valid := Parser.Parse (Directory & "expression_n_05.adb");
PASS    exception DXPL.Compiler.Loop_Out_Of_Range

-------------------------
--  POSITIVE EXAMPLES  --
-------------------------

*****   expression_p_00.adb
TEST    Valid := Parser.Parse (Directory & "expression_p_00.adb");
PASS    Valid = True

*****   expression_p_01.adb
TEST    Valid := Parser.Parse (Directory & "expression_p_01.adb");
PASS    Valid = True

*****   expression_p_02.adb
TEST    Valid := Parser.Parse (Directory & "expression_p_02.adb");
PASS    Valid = True

*****   expression_p_03.adb
TEST    Valid := Parser.Parse (Directory & "expression_p_03.adb");
PASS    Valid = True

*****   expression_p_04.adb
TEST    Valid := Parser.Parse (Directory & "expression_p_04.adb");
PASS    Valid = True
