--!  threefish512.adb
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
--!  Created       :: 05/13/2009
--!  Last-Modified :: 09/14/2009
--!
--!  Description of 'Threefish 512' with old rotation constants taken
--!  from the first submitting to the SHA-3 contest.
------------------------------------------------------------------------
procedure Threefish512 is

   -----------------------------------------------
   --  Constants used for rotation to the left  --
   -----------------------------------------------

   subtype Rotation_Constants is Integer range 0 .. 64;
   K : constant array (0 .. 31) of Rotation_Constants :=
     (0  => 38, 1  => 30, 2  => 50, 3  => 53,
      4  => 48, 5  => 20, 6  => 43, 7  => 31,
      8  => 34, 9  => 14, 10 => 15, 11 => 27,
      12 => 26, 13 => 12, 14 => 58, 15 =>  7,
      16 => 33, 17 => 49, 18 =>  8, 19 => 42,
      20 => 39, 21 => 27, 22 => 41, 23 => 14,
      24 => 29, 25 => 26, 26 => 11, 27 =>  9,
      28 => 33, 29 => 51, 30 => 39, 31 => 35);

   ---------------------------------------------
   --  Process eight rounds of Threefish-512  --
   ---------------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_8) is
   --  Unrolling 8 rounds
   begin
     --# BEGIN
     --  1
     Message (0) := Message (0) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K  (0));
     Message (1) := Message (1) xor Message (0);
     Message (2) := Message (2) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K  (1));
     Message (3) := Message (3) xor Message (2);
     Message (4) := Message (4) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K  (2));
     Message (5) := Message (5) xor Message (4);
     Message (6) := Message (6) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K  (3));
     Message (7) := Message (7) xor Message (6);

     --  2
     Message (2) := Message (2) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K  (4));
     Message (1) := Message (1) xor Message (2);
     Message (4) := Message (4) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K  (5));
     Message (7) := Message (7) xor Message (4);
     Message (6) := Message (6) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K  (6));
     Message (5) := Message (5) xor Message (6);
     Message (0) := Message (0) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K  (7));
     Message (3) := Message (3) xor Message (0);

     --  3
     Message (4) := Message (4) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K  (8));
     Message (1) := Message (1) xor Message (4);
     Message (6) := Message (6) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K  (9));
     Message (3) := Message (3) xor Message (6);
     Message (0) := Message (0) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K (10));
     Message (5) := Message (5) xor Message (0);
     Message (2) := Message (2) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K (11));
     Message (7) := Message (7) xor Message (2);

     --  4
     Message (6) := Message (6) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K (12));
     Message (1) := Message (1) xor Message (6);
     Message (0) := Message (0) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K (13));
     Message (7) := Message (7) xor Message (0);
     Message (2) := Message (2) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K (14));
     Message (5) := Message (5) xor Message (2);
     Message (4) := Message (4) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K (15));
     Message (3) := Message (3) xor Message (4);
     --  key injection omitted

     --  5
     Message (0) := Message (0) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K (16));
     Message (1) := Message (1) xor Message (0);
     Message (2) := Message (2) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K (17));
     Message (3) := Message (3) xor Message (2);
     Message (4) := Message (4) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K (18));
     Message (5) := Message (5) xor Message (4);
     Message (6) := Message (6) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K (19));
     Message (7) := Message (7) xor Message (6);

     --  6
     Message (2) := Message (2) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K (20));
     Message (1) := Message (1) xor Message (2);
     Message (4) := Message (4) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K (21));
     Message (7) := Message (7) xor Message (4);
     Message (6) := Message (6) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K (22));
     Message (5) := Message (5) xor Message (6);
     Message (0) := Message (0) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K (23));
     Message (3) := Message (3) xor Message (0);

     --  7
     Message (4) := Message (4) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K (24));
     Message (1) := Message (1) xor Message (4);
     Message (6) := Message (6) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K (25));
     Message (3) := Message (3) xor Message (6);
     Message (0) := Message (0) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K (26));
     Message (5) := Message (5) xor Message (0);
     Message (2) := Message (2) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K (27));
     Message (7) := Message (7) xor Message (2);

     --  8
     Message (6) := Message (6) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), K (28));
     Message (1) := Message (1) xor Message (6);
     Message (0) := Message (0) + Message (7);
     Message (7) := DXPL_Types_64.Rotate_Left (Message (7), K (29));
     Message (7) := Message (7) xor Message (0);
     Message (2) := Message (2) + Message (5);
     Message (5) := DXPL_Types_64.Rotate_Left (Message (5), K (30));
     Message (5) := Message (5) xor Message (2);
     Message (4) := Message (4) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), K (31));
     Message (3) := Message (3) xor Message (4);
     --  key injection omitted
     --# END
   end DXPL_Process;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "Threefish 512",
      DXPL_ROUNDS      => 9,
      DXPL_TERMINATION => 512);

   --  Input Message 'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   --                'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0 => 16#e7cdab1289e7cdab#,
                       1 => 16#ab1289e7cdab1289#,
                       2 => 16#89e7cdab1289e7cd#,
                       3 => 16#af231289e7cdab12#,
                       4 => 16#e7cdab1289e7cdab#,
                       5 => 16#ab1289e7cdab1289#,
                       6 => 16#89e7cdab1289e7cd#,
                       7 => 16#af231289e7cdab12#),
      DXPL_DIGEST  => (0 => 16#2bdea02986c1a804#,
                       1 => 16#9f5d2590cc2108ea#,
                       2 => 16#99fc99ac6ac51cf#,
                       3 => 16#369738bbf5afb10e#,
                       4 => 16#3cbf1196dcc879e3#,
                       5 => 16#5597eb1607e27bb2#,
                       6 => 16#24ec106a3ddc29ac#,
                       7 => 16#9a2705f60b17e116#));

   --  Input Message 'abcde78912abcde78912abcde78912abcde78912abcde78912abcde7891223af'
   --                '00007FFF5FBFEE800000000100014EBB00000000000000000000000000000000'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0 => 16#e7cdab1289e7cdab#,
                       1 => 16#ab1289e7cdab1289#,
                       2 => 16#89e7cdab1289e7cd#,
                       3 => 16#af231289e7cdab12#,
                       4 => 16#80eebf5fff7f0000#,
                       5 => 16#bb4e010001000000#,
                       6 => 16#0000000000000000#,
                       7 => 16#0000000000000000#),
      DXPL_DIGEST  => (0 => 16#50794e200d7b6894#,
                       1 => 16#3ff3f5306ecd0ef3#,
                       2 => 16#2352d3d36bf19279#,
                       3 => 16#8c26a022855ba395#,
                       4 => 16#836866039f3eb04d#,
                       5 => 16#d42ebded7ebb383c#,
                       6 => 16#ea00836f717a9e6b#,
                       7 => 16#08301b277599b8e6#));
                
end Threefish512;

