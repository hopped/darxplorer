--!  tea.adb
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
--!  Created       :: 05/12/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of Tiny Encription Algorithm (TEA) based on the
--!  reference implementation.
------------------------------------------------------------------------
procedure TEA is

   ------------------------------
   --  Additional definitions  --
   ------------------------------

   TEA_Delta : DXPL_Types_32.Word := 16#9E3779B9#;
   TEA_Sum   : DXPL_Types_32.Word := 2#0#;

   --------------------------------
   --  Process one round of TEA  --
   --------------------------------

   procedure DXPL_Process
     (Message : in out DXPL_Types_32.Word_Array_2;
      Key     : in DXPL_Types_32.Word_Array_4 := (others => 16#0#)) is
   begin
      --# BEGIN
      TEA_Sum := TEA_Sum + TEA_Delta;
      Message (0) := Message (0) +
                     ((Shift_Left (Message (1), 4) + Key (0)) xor
                      (Message (1) + TEA_Sum) xor
                      (Shift_Right (Message (1), 5) + Key (1)));
      Message (1) := Message (1) +
                     ((Shift_Left (Message (0), 4) + Key (2)) xor
                      (Message (0) + TEA_Sum) xor
                      (Shift_Right (Message (0), 5) + Key (3)));
      --# END
   end DXPL_Process;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_32.Configuration
     (DXPL_ALGORITHM   => "Tiny Encryption Algorithm (TEA)",
      DXPL_ROUNDS      => 32,
      DXPL_TERMINATION => 256);

   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0 => 16#01234567#, 1 => 16#89abcdef#),
      DXPL_KEY     => (0 => 16#00112233#, 1 => 16#44556677#,
                       2 => 16#8899aabb#, 3 => 16#ccddeeff#),
      DXPL_DIGEST  => (0 => 16#126c6b92#, 1 => 16#c0653a3e#));
end TEA;
