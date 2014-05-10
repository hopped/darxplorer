------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/20/2009
--!  Last-Modified :: 09/20/2009

--!  Purpose:
--!  Providing tests for the branch
--!    * proc_decl

--!  File was tested with tg-3.1
--!  See <http://www.free-software-consulting.com/projects/tg/>
------------------------------------------------------------------------
ERROR_HANDLING  stop

CONTEXT with Ada.Text_IO;  use Ada.Text_IO;
        with DXPL.Compiler.Parser;
        with DXPL.Compiler.Support.Annotation;

DEFINE  package Parser is new DXPL.Compiler.Parser;
        package Annotation renames DXPL.Compiler.Support.Annotation;
        Directory : String := "./tests/data/proc_decl/";
        Valid  : Boolean := False;

-------------------------
--  NEGATIVE EXAMPLES  --
-------------------------

*****   proc_decl_n_00.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_n_00.adb");
PASS    exception DXPL.Compiler.DXPL_Procedure_Exception

*****   proc_decl_n_01.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_n_01.adb");
PASS    exception DXPL.Compiler.DXPL_Procedure_Exception

-------------------------
--  POSITIVE EXAMPLES  --
-------------------------

*****   proc_decl_p_00.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_00.adb");
PASS    Valid = True

*****   proc_decl_p_01.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_01.adb");
PASS    Valid = True

*****   proc_decl_p_02.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_02.adb");
PASS    Valid = True

*****   proc_decl_p_03.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_03.adb");
PASS    Valid = True

*****   proc_decl_p_04.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_04.adb");
PASS    Valid = True

*****   proc_decl_p_05.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_05.adb");
PASS    Valid = True

*****   proc_decl_p_06.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_06.adb");
PASS    Valid = True

*****   proc_decl_p_07.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_07.adb");
PASS    Valid = True

*****   proc_decl_p_08.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_08.adb");
PASS    Valid = True

*****   proc_decl_p_09.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_09.adb");
PASS    Valid = True

*****   proc_decl_p_10.adb
TEST    Valid := Parser.Parse (Directory & "proc_decl_p_10.adb");
PASS    Valid = True
