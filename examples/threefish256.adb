--!  threefish256.adb
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
--!  Created       :: 05/11/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of 'Threefish 256' with old rotation constants.
------------------------------------------------------------------------
procedure Threefish256 is

   -----------------------------------------------
   --  Constants used for rotation to the left  --
   -----------------------------------------------

   subtype Rotation_Constants is Integer range 0 .. 64;
    --  Skein_256 round rotation constants
   R_256_0_0 : Rotation_Constants :=  5;
   R_256_0_1 : Rotation_Constants := 56;
   R_256_1_0 : Rotation_Constants := 36;
   R_256_1_1 : Rotation_Constants := 28;
   R_256_2_0 : Rotation_Constants := 13;
   R_256_2_1 : Rotation_Constants := 46;
   R_256_3_0 : Rotation_Constants := 58;
   R_256_3_1 : Rotation_Constants := 44;
   R_256_4_0 : Rotation_Constants := 26;
   R_256_4_1 : Rotation_Constants := 20;
   R_256_5_0 : Rotation_Constants := 53;
   R_256_5_1 : Rotation_Constants := 35;
   R_256_6_0 : Rotation_Constants := 11;
   R_256_6_1 : Rotation_Constants := 42;
   R_256_7_0 : Rotation_Constants := 59;
   R_256_7_1 : Rotation_Constants := 50;

   ---------------------------------------------
   --  Process eight rounds of Threefish-256  --
   ---------------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_4) is
   --  Unrolling eight rounds
   begin
     --# BEGIN
     -- 1
     Message (0) := Message (0) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_0_0);
     Message (1) := Message (1) xor Message (0);
     Message (2) := Message (2) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_0_1);
     Message (3) := Message (3) xor Message (2);

     -- 2
     Message (0) := Message (0) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_1_0);
     Message (3) := Message (3) xor Message (0);
     Message (2) := Message (2) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_1_1);
     Message (1) := Message (1) xor Message (2);

     -- 3
     Message (0) := Message (0) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_2_0);
     Message (1) := Message (1) xor Message (0);
     Message (2) := Message (2) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_2_1);
     Message (3) := Message (3) xor Message (2);

     -- 4
     Message (0) := Message (0) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_3_0);
     Message (3) := Message (3) xor Message (0);
     Message (2) := Message (2) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_3_1);
     Message (1) := Message (1) xor Message (2);
     --  key injection omitted

     -- 5
     Message (0) := Message (0) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_4_0);
     Message (1) := Message (1) xor Message (0);
     Message (2) := Message (2) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_4_1);
     Message (3) := Message (3) xor Message (2);
     
     -- 6
     Message (0) := Message (0) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_5_0);
     Message (3) := Message (3) xor Message (0);
     Message (2) := Message (2) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_5_1);
     Message (1) := Message (1) xor Message (2);
     
     -- 7
     Message (0) := Message (0) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_6_0);
     Message (1) := Message (1) xor Message (0);
     Message (2) := Message (2) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_6_1);
     Message (3) := Message (3) xor Message (2);
     
     -- 8
     Message (0) := Message (0) + Message (3);
     Message (3) := DXPL_Types_64.Rotate_Left (Message (3), R_256_7_0);
     Message (3) := Message (3) xor Message (0);
     Message (2) := Message (2) + Message (1);
     Message (1) := DXPL_Types_64.Rotate_Left (Message (1), R_256_7_1);
     Message (1) := Message (1) xor Message (2); 
     --  key injection omitted
     --# END
   end DXPL_Process;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "Threefish 256",
      DXPL_ROUNDS      => 9,
      DXPL_TERMINATION => 256);

   --  Input Message
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0 => 16#00007FFF5FBFEE80#,
                       1 => 16#0000000100014EBB#,
                       2 => 16#0000000000000000#,
                       3 => 16#0000000000000000#),
      DXPL_DIGEST  => (0 => 16#A269F1AC18B505A0#,
                       1 => 16#B53E6BADAC3DA639#,
                       2 => 16#82AE1EE23B82FED6#,
                       3 => 16#BB63392C75D1E39D#));

   --  Input Message '00007FFF5FBFEE800000000100014EBB'
   --                '00000000000000000000000000000000'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0 => 16#80eebf5fff7f0000#,
                       1 => 16#bb4e010001000000#,
                       2 => 16#0000000000000000#,
                       3 => 16#0000000000000000#),
      DXPL_DIGEST  => (0 => 16#3457e7f138394656#,
                       1 => 16#4b23e32231b18ba3#,
                       2 => 16#ad8e6d1d759e6229#,
                       3 => 16#6503d98526096ae8#));

   --  Input Message 'abcde78912abcde78912abcde78912ab'
   --                'cde78912abcde78912abcde7891223af'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0 => 16#e7cdab1289e7cdab#,
                       1 => 16#ab1289e7cdab1289#,
                       2 => 16#89e7cdab1289e7cd#,
                       3 => 16#af231289e7cdab12#),
      DXPL_DIGEST  => (0 => 16#6252deca58a00fd9#,
                       1 => 16#93db7650bc9aa678#,
                       2 => 16#d9e8bd2d2ac14c02#,
                       3 => 16#cab7b5c75f1cadb5#));
end Threefish256;
