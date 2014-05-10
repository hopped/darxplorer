--!  edonr512.adb
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
--!  Created       :: 09/16/2009
--!  Last-Modified :: 09/16/2009
--!
--!  Description of 'EDON-R 512'. It is based on the first
--!  subscription to NIST.
------------------------------------------------------------------------

procedure EdonR512 is

   -------------------------------------------------
   --  Initial double chaining pipe of Edon-R512  --
   -------------------------------------------------

   IV : DXPL_Types_64.Word_Array_16 :=
     (16#8081828384858687#,
      16#88898a8b8c8d8e8f#,
      16#9091929394959697#,
      16#98999a9b9c9d9e9f#,
      16#a0a1a2a3a4a5a6a7#,
      16#a8a9aaabacadaeaf#,
      16#b0b1b2b3b4b5b6b7#,
      16#b8b9babbbcbdbebf#,
      16#c0c1c2c3c4c5c6c7#,
      16#c8c9cacbcccdcecf#,
      16#d0d1d2d3d4d5d6d7#,
      16#d8d9dadbdcdddedf#,
      16#e0e1e2e3e4e5e6e7#,
      16#e8e9eaebecedeeef#,
      16#f0f1f2f3f4f5f6f7#,
      16#f8f9fafbfcfdfeff#);

   M : DXPL_Types_64.Word_Array_16 := (others => 16#0#);
   P : DXPL_Types_64.Word_Array_32 := (others => 16#0#);
   T : DXPL_Types_64.Word_Array_16 := (others => 16#0#);

   --------------------------
   -- Quasigroup operation --
   --------------------------

   procedure Q512 (X : in DXPL_Types_64.Word_Array_8; Y, Z : in out DXPL_Types_64.Word_Array_8) is
   begin
      --# BEGIN
      -- First Latin Square
      -- 0   7   1   3   2   4   6   5
      -- 4   1   7   6   3   0   5   2
      -- 7   0   4   2   5   3   1   6
      -- 1   4   0   5   6   2   7   3
      -- 2   3   6   7   1   5   0   4
      -- 5   2   3   1   7   6   4   0
      -- 3   6   5   0   4   7   2   1
      -- 6   5   2   4   0   1   3   7
      T ( 0) := 16#aaaaaaaaaaaaaaaa# + X (0) + X (1) + X (2) + X (4) + X (7);
      T ( 1) := X (0) + X (1) + X (3) + X (4) + X (7);
      T ( 2) := X (0) + X (1) + X (4) + X (6) + X (7);
      T ( 3) := X (2) + X (3) + X (5) + X (6) + X (7);
      T ( 4) := X (1) + X (2) + X (3) + X (5) + X (6);
      T ( 5) := X (0) + X (2) + X (3) + X (4) + X (5);
      T ( 6) := X (0) + X (1) + X (5) + X (6) + X (7);
      T ( 7) := X (2) + X (3) + X (4) + X (5) + X (6);
      T ( 1) := DXPL_Types_64.Rotate_Left (T (1),  5);
      T ( 2) := DXPL_Types_64.Rotate_Left (T (2), 19);
      T ( 3) := DXPL_Types_64.Rotate_Left (T (3), 29);
      T ( 4) := DXPL_Types_64.Rotate_Left (T (4), 31);
      T ( 5) := DXPL_Types_64.Rotate_Left (T (5), 41);
      T ( 6) := DXPL_Types_64.Rotate_Left (T (6), 57);
      T ( 7) := DXPL_Types_64.Rotate_Left (T (7), 61);

      T (8 ) := T (3) xor T (5) xor T (6);
      T (9 ) := T (2) xor T (5) xor T (6);
      T (10) := T (2) xor T (3) xor T (5);
      T (11) := T (0) xor T (1) xor T (4);
      T (12) := T (0) xor T (4) xor T (7);
      T (13) := T (1) xor T (6) xor T (7);
      T (14) := T (2) xor T (3) xor T (4);
      T (15) := T (0) xor T (1) xor T (7);

      -- Second Orthogonal Latin Square
      -- 0   4   2   3   1   6   5   7
      -- 7   6   3   2   5   4   1   0
      -- 5   3   1   6   0   2   7   4
      -- 1   0   5   4   3   7   2   6
      -- 2   1   0   7   4   5   6   3
      -- 3   5   7   0   6   1   4   2
      -- 4   7   6   1   2   0   3   5
      -- 6   2   4   5   7   3   0   1
      T (0) := 16#5555555555555555# + Y (0) + Y (1) + Y (2) + Y (5) + Y (7);
      T (1) := Y (0) + Y (1) + Y (3) + Y (4) + Y (6);
      T (2) := Y (0) + Y (1) + Y (2) + Y (3) + Y (5);
      T (3) := Y (2) + Y (3) + Y (4) + Y (6) + Y (7);
      T (4) := Y (0) + Y (1) + Y (3) + Y (4) + Y (5);
      T (5) := Y (2) + Y (4) + Y (5) + Y (6) + Y (7);
      T (6) := Y (1) + Y (2) + Y (5) + Y (6) + Y (7);
      T (7) := Y (0) + Y (3) + Y (4) + Y (6) + Y (7);
      T (1) := DXPL_Types_64.Rotate_Left (T (1),  3);
      T (2) := DXPL_Types_64.Rotate_Left (T (2), 17);
      T (3) := DXPL_Types_64.Rotate_Left (T (3), 23);
      T (4) := DXPL_Types_64.Rotate_Left (T (4), 31);
      T (5) := DXPL_Types_64.Rotate_Left (T (5), 37);
      T (6) := DXPL_Types_64.Rotate_Left (T (6), 45);
      T (7) := DXPL_Types_64.Rotate_Left (T (7), 59);

      Z (5) := T (8 ) + (T (3) xor T (4) xor T (6));
      Z (6) := T (9 ) + (T (2) xor T (5) xor T (7));
      Z (7) := T (10) + (T (4) xor T (6) xor T (7));
      Z (0) := T (11) + (T (0) xor T (1) xor T (5));
      Z (1) := T (12) + (T (2) xor T (6) xor T (7));
      Z (2) := T (13) + (T (0) xor T (1) xor T (3));
      Z (3) := T (14) + (T (0) xor T (3) xor T (4));
      Z (4) := T (15) + (T (1) xor T (2) xor T (5));
      --# END
   end Q512;

   ---------------------------------------------------
   --  Reverse the elements in a given Word_Vector  --
   ---------------------------------------------------

   function Reverse_Slice (Message : in DXPL_Types_64.Word_Array_8) return DXPL_Types_64.Word_Array_8 is
      Answer : DXPL_Types_64.Word_Array_8 := (others => 16#0#);
      Length : Integer := Message'Length - 1;
   begin
      for Index in reverse Message'Range loop
         Answer (Length - Index) := Message (Index);
      end loop;

      return Answer;
   end Reverse_Slice;

   procedure DXPL_Initialize (Message : in DXPL_Types_64.Word_Array_16) is
   begin
      M           := Message;
      P (0 .. 15) := IV;
   end DXPL_Initialize;

   -----------------------------------------
   --  Process a full round of Edon-R512  --
   -----------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_16) is
   begin
      --  First row of quasiqroup e-transformations
      Q512 (Reverse_Slice (Message (8 .. 15)), Message (0 .. 7), P (16 .. 23));
      Q512 (P (16 .. 23), Message (8 .. 15), P (24 .. 31));

      --  Second row of quasigroup e-transformations
      Q512 (P ( 8 .. 15), P (16 .. 23), P (16 .. 23));
      Q512 (P (16 .. 23), P (24 .. 31), P (24 .. 31));

      --  Third row of quasigroup e-transformations
      Q512 (P (16 .. 23), P ( 0 ..  7), P (16 .. 23));
      Q512 (P (24 .. 31), P (16 .. 23), P (24 .. 31));

      --  Fourth row of quasigroup e-transformations
      Q512 (Reverse_Slice (Message (0 .. 7)), P (16 .. 23), P (0 .. 7));
      Q512 (P (0 .. 7), P (24 .. 31), P (8 .. 15));
   end DXPL_Process;

   procedure DXPL_Finalize (Message : in out DXPL_Types_64.Word_Array_16) is
   begin
      Message := P (0 .. 15);
   end DXPL_Finalize;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "Edon-R 512",
      DXPL_ROUNDS      => 1,
      DXPL_TERMINATION => 512);

   --  Input Message: 'abc'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#0000000080636261#,
                       1  => 16#0000000000000000#,
                       2  => 16#0000000000000000#,
                       3  => 16#0000000000000000#,
                       4  => 16#0000000000000000#,
                       5  => 16#0000000000000000#,
                       6  => 16#0000000000000000#,
                       7  => 16#0000000000000000#,
                       8  => 16#0000000000000000#,
                       9  => 16#0000000000000000#,
                       10 => 16#0000000000000000#,
                       11 => 16#0000000000000000#,
                       12 => 16#0000000000000000#,
                       13 => 16#0000000000000000#,
                       14 => 16#0000000000000000#,
                       15 => 16#0000000000000018#),
      DXPL_DIGEST  => (0  => 16#96681D19E64BEB2F#,
                       1  => 16#7DA17E87AC7A924A#,
                       2  => 16#2CC532E54B4C0F5E#,
                       3  => 16#15DA02A6ADB2DAA9#,
                       4  => 16#42CDB2AF425850A1#,
                       5  => 16#0EEB46F36C7CF6D4#,
                       6  => 16#100E893C99612155#,
                       7  => 16#66A63DBD6DC4D007#,
                       8  => 16#D9450231FABC79FE#,
                       9  => 16#D09FB991BCA89D13#,
                       10 => 16#FD1DCA3A7F6F3222#,
                       11 => 16#FE715D12E4846CFB#,
                       12 => 16#58E3FC1AD4A1B69B#,
                       13 => 16#29780A22352847F8#,
                       14 => 16#C2E5C8BF2B6B14D5#,
                       15 => 16#A4C117B5A9607F62#));

   -- Input Message: 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijk'
   --                'lmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnmnopqrsmopqrstu'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#6867666564636261#,
                       1  => 16#6968676665646362#,
                       2  => 16#6A69686766656463#,
                       3  => 16#6B6A696867666564#,
                       4  => 16#6C6B6A6968676665#,
                       5  => 16#6D6C6B6A69686766#,
                       6  => 16#6E6D6C6B6A696867#,
                       7  => 16#6F6E6D6C6B6A6968#,
                       8  => 16#706F6E6D6C6B6A69#,
                       9  => 16#71706F6E6D6C6B6A#,
                       10 => 16#7271706F6E6D6C6B#,
                       11 => 16#737271706F6E6D6C#,
                       12 => 16#74737271706F6E6D#,
                       13 => 16#737271706F6E6D6E#,
                       14 => 16#7574737271706F6D#,
                       15 => 16#0000000000000080#),
      DXPL_DIGEST  => (0  => 16#A795D749C8FE5DC3#,
                       1  => 16#770B5E2B7653106D#,
                       2  => 16#80033403CDDDA01A#,
                       3  => 16#826CC7231040612D#,
                       4  => 16#EBEEF7EF4EAE2866#,
                       5  => 16#E8EFA124CF348C15#,
                       6  => 16#3C365AA5DFE5577A#,
                       7  => 16#75AB45A5C8786B02#,
                       8  => 16#486983DBC12E433D#,
                       9  => 16#F1001B51CF114559#,
                       10 => 16#1E325D6650A7D700#,
                       11 => 16#25673B912903E3F3#,
                       12 => 16#B7EE262E9388A6DA#,
                       13 => 16#5D7D9352C2E030E9#,
                       14 => 16#435DC348BF0DB099#,
                       15 => 16#2A4C13AD7CEDE76F#));
end EdonR512;
