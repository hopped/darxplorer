------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/20/2009
--!  Last-Modified :: 09/20/2009

--!  Purpose:
--!  Providing tests for the branch
--!    * begin_declaration

--!  File was tested with tg-3.1
--!  See <http://www.free-software-consulting.com/projects/tg/>
------------------------------------------------------------------------
ERROR_HANDLING  stop

CONTEXT with Ada.Text_IO;  use Ada.Text_IO;
        with DXPL.Compiler.Parser;
        with DXPL.Compiler.Support.Annotation;

DEFINE  package Parser is new DXPL.Compiler.Parser;
        package Annotation renames DXPL.Compiler.Support.Annotation;
        Directory : String := "./tests/data/begin_decl/";
        Valid  : Boolean := False;

-------------------------
--  NEGATIVE EXAMPLES  --
-------------------------

*****   begin_decl_n_00.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_n_00.adb");
PASS    exception DXPL.Compiler.Configuration_Is_Missing

*****   begin_decl_n_01.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_n_01.adb");
PASS    exception DXPL.Compiler.No_Test_Vectors

*****   begin_decl_n_02.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_n_02.adb");
PASS    exception DXPL.Compiler.Inconsistent_Vectors

*****   begin_decl_n_03.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_n_03.adb");
PASS    exception DXPL.Compiler.Configuration_Error

*****   begin_decl_n_04.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_n_04.adb");
PASS    exception DXPL.Compiler.Begin_Is_Missing

-------------------------
--  POSITIVE EXAMPLES  --
-------------------------

*****   begin_decl_p_00.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_p_00.adb");
PASS    Valid = True

*****   begin_decl_p_01.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_p_01.adb");
PASS    Valid = True

*****   begin_decl_p_02.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_p_02.adb");
PASS    Valid = True

*****   begin_decl_p_03.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_p_03.adb");
PASS    Valid = True

*****   begin_decl_p_04.adb
TEST    Valid := Parser.Parse (Directory & "begin_decl_p_04.adb");
PASS    Valid = True
