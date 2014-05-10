--!  salsa20.adb
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
with DXPL_Types_32; use DXPL_Types_32;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/11/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of 'Blue Midnight Wish (BMW) 256'
------------------------------------------------------------------------
procedure Salsa20 is

   -----------------------------------
   --  Additional type definitions  --
   -----------------------------------

   Original_Message : DXPL_Types_32.Word_Array_16 := (others => 16#0#);

   ---------------------------------
   --  Initialization of Salsa20  --
   ---------------------------------

   procedure DXPL_Initialize (Message : in DXPL_Types_32.Word_Array_16) is
   begin
      Original_Message := Message;
   end DXPL_Initialize;

   ------------------------------------
   --  Process one round of Salsa20  --
   ------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
      --# BEGIN
      Message  (4) := Message  (4) xor Rotate_Left (Message  (0) + Message (12),  7);
      Message  (8) := Message  (8) xor Rotate_Left (Message  (4) + Message  (0),  9);
      Message (12) := Message (12) xor Rotate_Left (Message  (8) + Message  (4), 13);
      Message  (0) := Message  (0) xor Rotate_Left (Message (12) + Message  (8), 18);
      Message  (9) := Message  (9) xor Rotate_Left (Message  (5) + Message  (1),  7);
      Message (13) := Message (13) xor Rotate_Left (Message  (9) + Message  (5),  9);
      Message  (1) := Message  (1) xor Rotate_Left (Message (13) + Message  (9), 13);
      Message  (5) := Message  (5) xor Rotate_Left (Message  (1) + Message (13), 18);
      Message (14) := Message (14) xor Rotate_Left (Message (10) + Message  (6),  7);
      Message  (2) := Message  (2) xor Rotate_Left (Message (14) + Message (10),  9);
      Message  (6) := Message  (6) xor Rotate_Left (Message  (2) + Message (14), 13);
      Message (10) := Message (10) xor Rotate_Left (Message  (6) + Message  (2), 18);
      Message  (3) := Message  (3) xor Rotate_Left (Message (15) + Message (11),  7);
      Message  (7) := Message  (7) xor Rotate_Left (Message  (3) + Message (15),  9);
      Message (11) := Message (11) xor Rotate_Left (Message  (7) + Message  (3), 13);
      Message (15) := Message (15) xor Rotate_Left (Message (11) + Message  (7), 18);

      Message  (1) := Message  (1) xor Rotate_Left (Message  (0) + Message  (3),  7);
      Message  (2) := Message  (2) xor Rotate_Left (Message  (1) + Message  (0),  9);
      Message  (3) := Message  (3) xor Rotate_Left (Message  (2) + Message  (1), 13);
      Message  (0) := Message  (0) xor Rotate_Left (Message  (3) + Message  (2), 18);
      Message  (6) := Message  (6) xor Rotate_Left (Message  (5) + Message  (4),  7);
      Message  (7) := Message  (7) xor Rotate_Left (Message  (6) + Message  (5),  9);
      Message  (4) := Message  (4) xor Rotate_Left (Message  (7) + Message  (6), 13);
      Message  (5) := Message  (5) xor Rotate_Left (Message  (4) + Message  (7), 18);
      Message (11) := Message (11) xor Rotate_Left (Message (10) + Message  (9),  7);
      Message  (8) := Message  (8) xor Rotate_Left (Message (11) + Message (10),  9);
      Message  (9) := Message  (9) xor Rotate_Left (Message  (8) + Message (11), 13);
      Message (10) := Message (10) xor Rotate_Left (Message  (9) + Message  (8), 18);
      Message (12) := Message (12) xor Rotate_Left (Message (15) + Message (14),  7);
      Message (13) := Message (13) xor Rotate_Left (Message (12) + Message (15),  9);
      Message (14) := Message (14) xor Rotate_Left (Message (13) + Message (12), 13);
      Message (15) := Message (15) xor Rotate_Left (Message (14) + Message (13), 18);
      --# END
   end DXPL_Process;

   -------------------
   --  Final steps  --
   -------------------

   procedure DXPL_Finalize (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
      for I in Message'Range loop
         Message (I) := Message (I) + Original_Message (I);
      end loop;
   end DXPL_Finalize;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_32.Configuration
     (DXPL_ALGORITHM   => "Salsa20",
      DXPL_ROUNDS      => 10,
      DXPL_TERMINATION => 256);

   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#00000001#,
                       1  => 16#00000000#,
                       2  => 16#00000000#,
                       3  => 16#00000000#,
                       4  => 16#00000000#,
                       5  => 16#00000000#,
                       6  => 16#00000000#,
                       7  => 16#00000000#,
                       8  => 16#00000000#,
                       9  => 16#00000000#,
                       10 => 16#00000000#,
                       11 => 16#00000000#,
                       12 => 16#00000000#,
                       13 => 16#00000000#,
                       14 => 16#00000000#,
                       15 => 16#00000000#),
      DXPL_DIGEST  => (0  => 16#1CD2C5B6#,
                       1  => 16#12E00046#,
                       2  => 16#30C96673#,
                       3  => 16#0CF7B89C#,
                       4  => 16#01C1035C#,
                       5  => 16#CFE87193#,
                       6  => 16#F0BC08AC#,
                       7  => 16#8FBF36EB#,
                       8  => 16#1F424D57#,
                       9  => 16#48054223#,
                       10 => 16#F92F5C52#,
                       11 => 16#973F4FC7#,
                       12 => 16#D3B4C91B#,
                       13 => 16#55E794B5#,
                       14 => 16#154E3D69#,
                       15 => 16#16F5A6A3#));
end Salsa20;
