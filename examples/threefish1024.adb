--!  threefish1024.adb
--!
--!  Copyright 2009 Bauhaus-University Weimar, Germany
--!                 Chair of Media Security / Stefan Lucks
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
--!  Created       :: 05/14/2009
--!  Last-Modified :: 09/14/2009
--!
--!  Description of 'Threefish 1024' with old rotation constants taken
--!  from the first submitting to the SHA-3 contest.
------------------------------------------------------------------------
procedure Threefish1024 is

   -----------------------------------------------
   --  Constants used for rotation to the left  --
   -----------------------------------------------
   
   subtype Rotation_Constants is Integer range 0 .. 64;
   R1024_0_0 : Rotation_Constants := 55;
   R1024_0_1 : Rotation_Constants := 43;
   R1024_0_2 : Rotation_Constants := 37;
   R1024_0_3 : Rotation_Constants := 40;
   R1024_0_4 : Rotation_Constants := 16;
   R1024_0_5 : Rotation_Constants := 22;
   R1024_0_6 : Rotation_Constants := 38;
   R1024_0_7 : Rotation_Constants := 12;
   R1024_1_0 : Rotation_Constants := 25;
   R1024_1_1 : Rotation_Constants := 25;
   R1024_1_2 : Rotation_Constants := 46;
   R1024_1_3 : Rotation_Constants := 13;
   R1024_1_4 : Rotation_Constants := 14;
   R1024_1_5 : Rotation_Constants := 13;
   R1024_1_6 : Rotation_Constants := 52;
   R1024_1_7 : Rotation_Constants := 57;
   R1024_2_0 : Rotation_Constants := 33;
   R1024_2_1 : Rotation_Constants :=  8;
   R1024_2_2 : Rotation_Constants := 18;
   R1024_2_3 : Rotation_Constants := 57;
   R1024_2_4 : Rotation_Constants := 21;
   R1024_2_5 : Rotation_Constants := 12;
   R1024_2_6 : Rotation_Constants := 32;
   R1024_2_7 : Rotation_Constants := 54;
   R1024_3_0 : Rotation_Constants := 34;
   R1024_3_1 : Rotation_Constants := 43;
   R1024_3_2 : Rotation_Constants := 25;
   R1024_3_3 : Rotation_Constants := 60;
   R1024_3_4 : Rotation_Constants := 44;
   R1024_3_5 : Rotation_Constants :=  9;
   R1024_3_6 : Rotation_Constants := 59;
   R1024_3_7 : Rotation_Constants := 34;
   R1024_4_0 : Rotation_Constants := 28;
   R1024_4_1 : Rotation_Constants :=  7;
   R1024_4_2 : Rotation_Constants := 47;
   R1024_4_3 : Rotation_Constants := 48;
   R1024_4_4 : Rotation_Constants := 51;
   R1024_4_5 : Rotation_Constants :=  9;
   R1024_4_6 : Rotation_Constants := 35;
   R1024_4_7 : Rotation_Constants := 41;
   R1024_5_0 : Rotation_Constants := 17;
   R1024_5_1 : Rotation_Constants :=  6;
   R1024_5_2 : Rotation_Constants := 18;
   R1024_5_3 : Rotation_Constants := 25;
   R1024_5_4 : Rotation_Constants := 43;
   R1024_5_5 : Rotation_Constants := 42;
   R1024_5_6 : Rotation_Constants := 40;
   R1024_5_7 : Rotation_Constants := 15;
   R1024_6_0 : Rotation_Constants := 58;
   R1024_6_1 : Rotation_Constants :=  7;
   R1024_6_2 : Rotation_Constants := 32;
   R1024_6_3 : Rotation_Constants := 45;
   R1024_6_4 : Rotation_Constants := 19;
   R1024_6_5 : Rotation_Constants := 18;
   R1024_6_6 : Rotation_Constants :=  2;
   R1024_6_7 : Rotation_Constants := 56;
   R1024_7_0 : Rotation_Constants := 47;
   R1024_7_1 : Rotation_Constants := 49;
   R1024_7_2 : Rotation_Constants := 27;
   R1024_7_3 : Rotation_Constants := 58;
   R1024_7_4 : Rotation_Constants := 37;
   R1024_7_5 : Rotation_Constants := 48;
   R1024_7_6 : Rotation_Constants := 53;
   R1024_7_7 : Rotation_Constants := 56;

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
      DXPL_DIGEST  => (0  => 16#612b1309181bbb05#,
                       1  => 16#c224f731151cd7f1#,
                       2  => 16#1afe5bb0595a4793#,
                       3  => 16#e6fcd462250b887a#,
                       4  => 16#2ca2ba19948e3766#,
                       5  => 16#2ce8411cd3c77008#,
                       6  => 16#24f4011c5e913b96#,
                       7  => 16#d77bd7f926a1ccfd#,
                       8  => 16#ad3a21864ea9a235#,
                       9  => 16#8221c74c9d142593#,
                       10 => 16#f14d12973f63f304#,
                       11 => 16#ae9d1628fde80762#,
                       12 => 16#8917f1ff48cc8667#,
                       13 => 16#e127ae640f593acc#,
                       14 => 16#c685e1f3b41c5f86#,
                       15 => 16#0b423b0a5f379191#));

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
      DXPL_DIGEST  => (0  => 16#7596af9c528853ac#,
                       1  => 16#8839c1b145cfde2e#,
                       2  => 16#5ee0df87953e25b0#,
                       3  => 16#bf15e04a2a909d60#,
                       4  => 16#beb32a4dedcd10b1#,
                       5  => 16#1020e56d1c2ac26a#,
                       6  => 16#d4356bd87521e3f0#,
                       7  => 16#60ab72007755c04b#,
                       8  => 16#715031628b6d8a9e#,
                       9  => 16#6a813b67eb92b0be#,
                       10 => 16#d74acef02f9ca687#,
                       11 => 16#bbd8268c7b0366c4#,
                       12 => 16#e1e023b99e7056c5#,
                       13 => 16#ceea1bc2bf165050#,
                       14 => 16#e3259ba5e90dd4f5#,
                       15 => 16#2ad0592fbc4dff8f#));
end Threefish1024;

