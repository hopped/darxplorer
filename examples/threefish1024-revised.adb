--!  threefish1024.adb
--!
--!  Copyright 2009 Chair of Media Security / Prof. Dr. Stefan Lucks
--!
--!  This program is free software; you can redistribute it and/or modify
--!  it under the terms of the GNU General Public License as published by
--!  the Free Software Foundation; either version 2 of the License, or
--!  (at your option) any later version.
--!
--!  This program is distributed in the hope that it will be useful,
--!  but WITHOUT ANY WARRANTY; without even the implied warranty of
--!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--!  GNU General Public License for more details.
--!
--!  You should have received a copy of the GNU General Public License
--!  along with this program; if not, write to the Free Software
--!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
--!  MA 02110-1301, USA.
with DXPL_Types_64;  use DXPL_Types_64;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/15/2009
--!  Last-Modified :: 09/15/2009
--!
--!  Description of 'Threefish 1024'. This description is a tweaked 
--!  version of Threefish with new rotation constants.
------------------------------------------------------------------------
procedure Threefish1024_Revised is

   -----------------------------------------------
   --  Constants used for rotation to the left  --
   -----------------------------------------------
   
   subtype Rotation_Constants is Integer range 0 .. 64;
   R1024_0_0 : Rotation_Constants := 24;
   R1024_0_1 : Rotation_Constants := 13;
   R1024_0_2 : Rotation_Constants :=  8;
   R1024_0_3 : Rotation_Constants := 47;
   R1024_0_4 : Rotation_Constants :=  8;
   R1024_0_5 : Rotation_Constants := 17;
   R1024_0_6 : Rotation_Constants := 22;
   R1024_0_7 : Rotation_Constants := 37;
   R1024_1_0 : Rotation_Constants := 38;
   R1024_1_1 : Rotation_Constants := 19;
   R1024_1_2 : Rotation_Constants := 10;
   R1024_1_3 : Rotation_Constants := 55;
   R1024_1_4 : Rotation_Constants := 49;
   R1024_1_5 : Rotation_Constants := 18;
   R1024_1_6 : Rotation_Constants := 23;
   R1024_1_7 : Rotation_Constants := 52;
   R1024_2_0 : Rotation_Constants := 33;
   R1024_2_1 : Rotation_Constants :=  4;
   R1024_2_2 : Rotation_Constants := 51;
   R1024_2_3 : Rotation_Constants := 13;
   R1024_2_4 : Rotation_Constants := 34;
   R1024_2_5 : Rotation_Constants := 41;
   R1024_2_6 : Rotation_Constants := 59;
   R1024_2_7 : Rotation_Constants := 17;
   R1024_3_0 : Rotation_Constants :=  5;
   R1024_3_1 : Rotation_Constants := 20;
   R1024_3_2 : Rotation_Constants := 48;
   R1024_3_3 : Rotation_Constants := 41;
   R1024_3_4 : Rotation_Constants := 47;
   R1024_3_5 : Rotation_Constants := 28;
   R1024_3_6 : Rotation_Constants := 16;
   R1024_3_7 : Rotation_Constants := 25;
   R1024_4_0 : Rotation_Constants := 41;
   R1024_4_1 : Rotation_Constants :=  9;
   R1024_4_2 : Rotation_Constants := 37;
   R1024_4_3 : Rotation_Constants := 31;
   R1024_4_4 : Rotation_Constants := 12;
   R1024_4_5 : Rotation_Constants := 47;
   R1024_4_6 : Rotation_Constants := 44;
   R1024_4_7 : Rotation_Constants := 30;
   R1024_5_0 : Rotation_Constants := 16;
   R1024_5_1 : Rotation_Constants := 34;
   R1024_5_2 : Rotation_Constants := 56;
   R1024_5_3 : Rotation_Constants := 51;
   R1024_5_4 : Rotation_Constants :=  4;
   R1024_5_5 : Rotation_Constants := 53;
   R1024_5_6 : Rotation_Constants := 42;
   R1024_5_7 : Rotation_Constants := 41;
   R1024_6_0 : Rotation_Constants := 31;
   R1024_6_1 : Rotation_Constants := 44;
   R1024_6_2 : Rotation_Constants := 47;
   R1024_6_3 : Rotation_Constants := 46;
   R1024_6_4 : Rotation_Constants := 19;
   R1024_6_5 : Rotation_Constants := 42;
   R1024_6_6 : Rotation_Constants := 44;
   R1024_6_7 : Rotation_Constants := 25;
   R1024_7_0 : Rotation_Constants :=  9;
   R1024_7_1 : Rotation_Constants := 48;
   R1024_7_2 : Rotation_Constants := 35;
   R1024_7_3 : Rotation_Constants := 52;
   R1024_7_4 : Rotation_Constants := 23;
   R1024_7_5 : Rotation_Constants := 31;
   R1024_7_6 : Rotation_Constants := 37;
   R1024_7_7 : Rotation_Constants := 20;

   ---------------------------------------------
   --  Process eight rounds of Threefish-512  --
   ---------------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_16) is
   --  Unrolling eight rounds
   begin
     --# BEGIN
     --  1
     Message ( 0) := Message ( 0) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_0_0); Message ( 1) := Message ( 1) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_0_1); Message ( 3) := Message ( 3) xor Message ( 2);
     Message ( 4) := Message ( 4) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_0_2); Message ( 5) := Message ( 5) xor Message ( 4);
     Message ( 6) := Message ( 6) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_0_3); Message ( 7) := Message ( 7) xor Message ( 6);
     Message ( 8) := Message ( 8) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_0_4); Message ( 9) := Message ( 9) xor Message ( 8);
     Message (10) := Message (10) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_0_5); Message (11) := Message (11) xor Message (10);
     Message (12) := Message (12) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_0_6); Message (13) := Message (13) xor Message (12);
     Message (14) := Message (14) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_0_7); Message (15) := Message (15) xor Message (14);

     --  2
     Message ( 0) := Message ( 0) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_1_0); Message ( 9) := Message ( 9) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_1_1); Message (13) := Message (13) xor Message ( 2);
     Message ( 6) := Message ( 6) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_1_2); Message (11) := Message (11) xor Message ( 6);
     Message ( 4) := Message ( 4) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_1_3); Message (15) := Message (15) xor Message ( 4);
     Message (10) := Message (10) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_1_4); Message ( 7) := Message ( 7) xor Message (10);
     Message (12) := Message (12) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_1_5); Message ( 3) := Message ( 3) xor Message (12);
     Message (14) := Message (14) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_1_6); Message ( 5) := Message ( 5) xor Message (14);
     Message ( 8) := Message ( 8) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_1_7); Message ( 1) := Message ( 1) xor Message ( 8);

     --  3
     Message ( 0) := Message ( 0) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_2_0); Message ( 7) := Message ( 7) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_2_1); Message ( 5) := Message ( 5) xor Message ( 2);
     Message ( 4) := Message ( 4) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_2_2); Message ( 3) := Message ( 3) xor Message ( 4);
     Message ( 6) := Message ( 6) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_2_3); Message ( 1) := Message ( 1) xor Message ( 6);
     Message (12) := Message (12) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_2_4); Message (15) := Message (15) xor Message (12);
     Message (14) := Message (14) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_2_5); Message (13) := Message (13) xor Message (14);
     Message ( 8) := Message ( 8) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_2_6); Message (11) := Message (11) xor Message ( 8);
     Message (10) := Message (10) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_2_7); Message ( 9) := Message ( 9) xor Message (10);

     --  4
     Message ( 0) := Message ( 0) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_3_0); Message (15) := Message (15) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_3_1); Message (11) := Message (11) xor Message ( 2);
     Message ( 6) := Message ( 6) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_3_2); Message (13) := Message (13) xor Message ( 6);
     Message ( 4) := Message ( 4) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_3_3); Message ( 9) := Message ( 9) xor Message ( 4);
     Message (14) := Message (14) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_3_4); Message ( 1) := Message ( 1) xor Message (14);
     Message ( 8) := Message ( 8) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_3_5); Message ( 5) := Message ( 5) xor Message ( 8);
     Message (10) := Message (10) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_3_6); Message ( 3) := Message ( 3) xor Message (10);
     Message (12) := Message (12) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_3_7); Message ( 7) := Message ( 7) xor Message (12);
     --  key injection omitted

     --  5
     Message ( 0) := Message ( 0) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_4_0); Message ( 1) := Message ( 1) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_4_1); Message ( 3) := Message ( 3) xor Message ( 2);
     Message ( 4) := Message ( 4) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_4_2); Message ( 5) := Message ( 5) xor Message ( 4);
     Message ( 6) := Message ( 6) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_4_3); Message ( 7) := Message ( 7) xor Message ( 6);
     Message ( 8) := Message ( 8) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_4_4); Message ( 9) := Message ( 9) xor Message ( 8);
     Message (10) := Message (10) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_4_5); Message (11) := Message (11) xor Message (10);
     Message (12) := Message (12) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_4_6); Message (13) := Message (13) xor Message (12);
     Message (14) := Message (14) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_4_7); Message (15) := Message (15) xor Message (14);

     --  6
     Message ( 0) := Message ( 0) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_5_0); Message ( 9) := Message ( 9) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_5_1); Message (13) := Message (13) xor Message ( 2);
     Message ( 6) := Message ( 6) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_5_2); Message (11) := Message (11) xor Message ( 6);
     Message ( 4) := Message ( 4) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_5_3); Message (15) := Message (15) xor Message ( 4);
     Message (10) := Message (10) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_5_4); Message ( 7) := Message ( 7) xor Message (10);
     Message (12) := Message (12) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_5_5); Message ( 3) := Message ( 3) xor Message (12);
     Message (14) := Message (14) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_5_6); Message ( 5) := Message ( 5) xor Message (14);
     Message ( 8) := Message ( 8) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_5_7); Message ( 1) := Message ( 1) xor Message ( 8);

     --  7
     Message ( 0) := Message ( 0) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_6_0); Message ( 7) := Message ( 7) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_6_1); Message ( 5) := Message ( 5) xor Message ( 2);
     Message ( 4) := Message ( 4) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_6_2); Message ( 3) := Message ( 3) xor Message ( 4);
     Message ( 6) := Message ( 6) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_6_3); Message ( 1) := Message ( 1) xor Message ( 6);
     Message (12) := Message (12) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_6_4); Message (15) := Message (15) xor Message (12);
     Message (14) := Message (14) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_6_5); Message (13) := Message (13) xor Message (14);
     Message ( 8) := Message ( 8) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_6_6); Message (11) := Message (11) xor Message ( 8);
     Message (10) := Message (10) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_6_7); Message ( 9) := Message ( 9) xor Message (10);

     --  8
     Message ( 0) := Message ( 0) + Message (15); Message (15) := DXPL_Types_64.Rotate_Left (Message (15), R1024_7_0); Message (15) := Message (15) xor Message ( 0);
     Message ( 2) := Message ( 2) + Message (11); Message (11) := DXPL_Types_64.Rotate_Left (Message (11), R1024_7_1); Message (11) := Message (11) xor Message ( 2);
     Message ( 6) := Message ( 6) + Message (13); Message (13) := DXPL_Types_64.Rotate_Left (Message (13), R1024_7_2); Message (13) := Message (13) xor Message ( 6);
     Message ( 4) := Message ( 4) + Message ( 9); Message ( 9) := DXPL_Types_64.Rotate_Left (Message ( 9), R1024_7_3); Message ( 9) := Message ( 9) xor Message ( 4);
     Message (14) := Message (14) + Message ( 1); Message ( 1) := DXPL_Types_64.Rotate_Left (Message ( 1), R1024_7_4); Message ( 1) := Message ( 1) xor Message (14);
     Message ( 8) := Message ( 8) + Message ( 5); Message ( 5) := DXPL_Types_64.Rotate_Left (Message ( 5), R1024_7_5); Message ( 5) := Message ( 5) xor Message ( 8);
     Message (10) := Message (10) + Message ( 3); Message ( 3) := DXPL_Types_64.Rotate_Left (Message ( 3), R1024_7_6); Message ( 3) := Message ( 3) xor Message (10);
     Message (12) := Message (12) + Message ( 7); Message ( 7) := DXPL_Types_64.Rotate_Left (Message ( 7), R1024_7_7); Message ( 7) := Message ( 7) xor Message (12);
     --  key injection omitted
     --# END
   end DXPL_Process;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "Threefish 1024",
      DXPL_ROUNDS      => 10,
      DXPL_TERMINATION => 1024);

   --  Input Message '101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f'
   --                '303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f'
   --                '505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f'
   --                '707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#1716151413121110#,
                       1  => 16#1f1e1d1c1b1a1918#,
                       2  => 16#2726252423222120#,
                       3  => 16#2f2e2d2c2b2a2928#,
                       4  => 16#3736353433323130#,
                       5  => 16#3f3e3d3c3b3a3938#,
                       6  => 16#4746454443424140#,
                       7  => 16#4f4e4d4c4b4a4948#,
                       8  => 16#5756555453525150#,
                       9  => 16#5f5e5d5c5b5a5958#,
                       10 => 16#6766656463626160#,
                       11 => 16#6f6e6d6c6b6a6968#,
                       12 => 16#7776757473727170#,
                       13 => 16#7f7e7d7c7b7a7978#,
                       14 => 16#8786858483828180#,
                       15 => 16#8f8e8d8c8b8a8988#),
      DXPL_DIGEST  => (0  => 16#47A376833BC41B9F#,
                       1  => 16#54ADDC9DF9B54315#,
                       2  => 16#307F1ADF5FD6058F#,
                       3  => 16#01CF85BE7C08D0CA#,
                       4  => 16#AC6F68C15723123F#,
                       5  => 16#BCEA1279D33E5583#,
                       6  => 16#04C91624B078E517#,
                       7  => 16#2E05FE74ABBE5B17#,
                       8  => 16#05E4DF46C8E9A288#,
                       9  => 16#B5A6832D87AA867D#,
                       10 => 16#5EE6DB2BCCEAB2D8#,
                       11 => 16#921923C0744859B3#,
                       12 => 16#E231F543AD53C9F5#,
                       13 => 16#F5B96691D3557BF8#,
                       14 => 16#EEC8697AE715A240#,
                       15 => 16#63835107C13E0A88#));

   --  Input Message 'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   --                'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   --                'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   --                'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#e7cdab1289e7cdab#,
                       1  => 16#ab1289e7cdab1289#,
                       2  => 16#89e7cdab1289e7cd#,
                       3  => 16#af231289e7cdab12#,
                       4  => 16#e7cdab1289e7cdab#,
                       5  => 16#ab1289e7cdab1289#,
                       6  => 16#89e7cdab1289e7cd#,
                       7  => 16#af231289e7cdab12#,
                       8  => 16#e7cdab1289e7cdab#,
                       9  => 16#ab1289e7cdab1289#,
                       10 => 16#89e7cdab1289e7cd#,
                       11 => 16#af231289e7cdab12#,
                       12 => 16#e7cdab1289e7cdab#,
                       13 => 16#ab1289e7cdab1289#,
                       14 => 16#89e7cdab1289e7cd#,
                       15 => 16#af231289e7cdab12#),
      DXPL_DIGEST  => (0  => 16#72CFBE135F6E9DA8#,
                       1  => 16#3E19E6CEFDBCAAD6#,
                       2  => 16#B35F1EB676312F23#,
                       3  => 16#6F780FF1B53F74C0#,
                       4  => 16#88D78110E69F15FE#,
                       5  => 16#929CCFE612C31513#,
                       6  => 16#82602A618AA1DE00#,
                       7  => 16#0BCF8A8A1D9F84E0#,
                       8  => 16#8516EA862812E440#,
                       9  => 16#05E13791A0664682#,
                       10 => 16#7D4955881594AC0C#,
                       11 => 16#F14A6A4182E4AF00#,
                       12 => 16#644A03A409DBEC18#,
                       13 => 16#6A71341952B25831#,
                       14 => 16#5D37BA2EAFF0F533#,
                       15 => 16#FA786666329B0520#));
end Threefish1024_Revised;
