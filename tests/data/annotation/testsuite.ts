------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/19/2009
--!  Last-Modified :: 09/19/2009

--!  Purpose:
--!  Providing tests for the branch
--!    * annotation

--!  File was tested with tg-3.1
--!  See <http://www.free-software-consulting.com/projects/tg/>
------------------------------------------------------------------------
ERROR_HANDLING  stop

CONTEXT with Ada.Text_IO;  use Ada.Text_IO;
        with DXPL.Compiler.Parser;
        with DXPL.Compiler.Support.Annotation;

DEFINE  package Parser is new DXPL.Compiler.Parser;
        package Annotation renames DXPL.Compiler.Support.Annotation;
        Directory : String := "./tests/data/annotation/";
        Valid  : Boolean := False;

-------------------------
--  NEGATIVE EXAMPLES  --
-------------------------

*****   annotation_n_00.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_00.adb");
PASS    exception Annotation.Missing_Annotation

*****   annotation_n_01.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_01.adb");
PASS    exception Annotation.Missing_Annotation

*****   annotation_n_02.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_02.adb");
PASS    exception Annotation.Missing_Annotation

*****   annotation_n_03.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_03.adb");
PASS    exception Annotation.Wrong_Nesting_of_Annotations;

*****   annotation_n_04.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_04.adb");
PASS    exception Annotation.Missing_Annotation

*****   annotation_n_05.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_05.adb");
PASS    exception Annotation.Wrong_Nesting_of_Annotations;

*****   annotation_n_06.adb
TEST    Valid := Parser.Parse (Directory & "annotation_n_06.adb");
PASS    exception Annotation.Wrong_Nesting_of_Annotations;

-------------------------
--  POSITIVE EXAMPLES  --
-------------------------

*****   annotation_p_00.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_00.adb");
PASS    Valid = True

*****   annotation_p_01.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_01.adb");
PASS    Valid = True

*****   annotation_p_02.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_02.adb");
PASS    Valid = True

*****   annotation_p_03.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_03.adb");
PASS    Valid = True

*****   annotation_p_04.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_04.adb");
PASS    Valid = True

*****   annotation_p_05.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_05.adb");
PASS    Valid = True

*****   annotation_p_06.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_06.adb");
PASS    Valid

*****   annotation_p_07.adb
TEST    Valid := Parser.Parse (Directory & "annotation_p_07.adb");
PASS    Valid
