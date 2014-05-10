--!  edonr256.adb
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
with DXPL_Types_32;  use DXPL_Types_32;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/11/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of 'EDON-R 256'. It is based on the first
--!  subscription to NIST.
------------------------------------------------------------------------

procedure EdonR256 is

   -------------------------------------------------
   --  Initial double chaining pipe of Edon-R256  --
   -------------------------------------------------

   IV : DXPL_Types_32.Word_Array_16 :=
     (16#40414243#,
      16#44454647#,
      16#48494a4b#,
      16#4c4d4e4f#,
      16#50515253#,
      16#54555657#,
      16#58595a5b#,
      16#5c5d5e5f#,
      16#60616263#,
      16#64656667#,
      16#68696a6b#,
      16#6c6d6e6f#,
      16#70717273#,
      16#74757677#,
      16#78797a7b#,
      16#7c7d7e7f#);

   M : DXPL_Types_32.Word_Array_16 := (others => 16#0#);
   P : DXPL_Types_32.Word_Array_32 := (others => 16#0#);
   T : DXPL_Types_32.Word_Array_16 := (others => 16#0#);

   --------------------------
   -- Quasigroup operation --
   --------------------------

   procedure Q256 (X : in DXPL_Types_32.Word_Array_8; Y, Z : in out DXPL_Types_32.Word_Array_8) is
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
      T (0) := 16#AAAAAAAA# + X (0) + X (1) + X (2) + X (4) + X (7);
      T (1) := X (0) + X (1) + X (3) + X (4) + X (7);
      T (2) := X (0) + X (1) + X (4) + X (6) + X (7);
      T (3) := X (2) + X (3) + X (5) + X (6) + X (7);
      T (4) := X (1) + X (2) + X (3) + X (5) + X (6);
      T (5) := X (0) + X (2) + X (3) + X (4) + X (5);
      T (6) := X (0) + X (1) + X (5) + X (6) + X (7);
      T (7) := X (2) + X (3) + X (4) + X (5) + X (6);
      T (1) := DXPL_Types_32.Rotate_Left (T (1),  5);
      T (2) := DXPL_Types_32.Rotate_Left (T (2), 11);
      T (3) := DXPL_Types_32.Rotate_Left (T (3), 13);
      T (4) := DXPL_Types_32.Rotate_Left (T (4), 17);
      T (5) := DXPL_Types_32.Rotate_Left (T (5), 19);
      T (6) := DXPL_Types_32.Rotate_Left (T (6), 29);
      T (7) := DXPL_Types_32.Rotate_Left (T (7), 31);

      T (8)  := T (3) xor T (5) xor T (6);
      T (9)  := T (2) xor T (5) xor T (6);
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
      T (0) := 16#55555555# + Y (0) + Y (1) + Y (2) + Y (5) + Y (7);
      T (1) := Y (0) + Y (1) + Y (3) + Y (4) + Y (6);
      T (2) := Y (0) + Y (1) + Y (2) + Y (3) + Y (5);
      T (3) := Y (2) + Y (3) + Y (4) + Y (6) + Y (7);
      T (4) := Y (0) + Y (1) + Y (3) + Y (4) + Y (5);
      T (5) := Y (2) + Y (4) + Y (5) + Y (6) + Y (7);
      T (6) := Y (1) + Y (2) + Y (5) + Y (6) + Y (7);
      T (7) := Y (0) + Y (3) + Y (4) + Y (6) + Y (7);
      T (1) := DXPL_Types_32.Rotate_Left (T (1),  3);
      T (2) := DXPL_Types_32.Rotate_Left (T (2),  7);
      T (3) := DXPL_Types_32.Rotate_Left (T (3), 11);
      T (4) := DXPL_Types_32.Rotate_Left (T (4), 17);
      T (5) := DXPL_Types_32.Rotate_Left (T (5), 19);
      T (6) := DXPL_Types_32.Rotate_Left (T (6), 23);
      T (7) := DXPL_Types_32.Rotate_Left (T (7), 29);

      Z (5) := T  (8) + (T (3) xor T (4) xor T (6));
      Z (6) := T  (9) + (T (2) xor T (5) xor T (7));
      Z (7) := T (10) + (T (4) xor T (6) xor T (7));
      Z (0) := T (11) + (T (0) xor T (1) xor T (5));
      Z (1) := T (12) + (T (2) xor T (6) xor T (7));
      Z (2) := T (13) + (T (0) xor T (1) xor T (3));
      Z (3) := T (14) + (T (0) xor T (3) xor T (4));
      Z (4) := T (15) + (T (1) xor T (2) xor T (5));
      --# END
   end Q256;

   ---------------------------------------------------
   --  Reverse the elements in a given Word_Vector  --
   ---------------------------------------------------

   function Reverse_Slice (Message : in DXPL_Types_32.Word_Array_8) return DXPL_Types_32.Word_Array_8 is
      Answer : DXPL_Types_32.Word_Array_8 := (others => 16#0#);
      Length : Integer := Message'Length - 1;
   begin
      for Index in reverse Message'Range loop
         Answer (Length - Index) := Message (Index);
      end loop;

      return Answer;
   end Reverse_Slice;

   procedure DXPL_Initialize (Message : in DXPL_Types_32.Word_Array_16) is
   begin
      M           := Message;
      P (0 .. 15) := IV;
   end DXPL_Initialize;

   -----------------------------------------
   --  Process a full round of Edon-R256  --
   -----------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
      --  First row of quasiqroup e-transformations
      Q256 (Reverse_Slice (Message (8 .. 15)), Message (0 .. 7), P (16 .. 23));
      Q256 (P (16 .. 23), Message (8 .. 15), P (24 .. 31));

      --  Second row of quasigroup e-transformations
      Q256 (P ( 8 .. 15), P (16 .. 23), P (16 .. 23));
      Q256 (P (16 .. 23), P (24 .. 31), P (24 .. 31));

      --  Third row of quasigroup e-transformations
      Q256 (P (16 .. 23), P ( 0 ..  7), P (16 .. 23));
      Q256 (P (24 .. 31), P (16 .. 23), P (24 .. 31));

      --  Fourth row of quasigroup e-transformations
      Q256 (Reverse_Slice (Message (0 .. 7)), P (16 .. 23), P (0 .. 7));
      Q256 (P (0 .. 7), P (24 .. 31), P (8 .. 15));
   end DXPL_Process;

   procedure DXPL_Finalize (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
      Message := P (0 .. 15);
   end DXPL_Finalize;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_32.Configuration
     (DXPL_ALGORITHM   => "Edon-R 256",
      DXPL_ROUNDS      => 1,
      DXPL_TERMINATION => 256);

   --  Input Message: "abc"
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#80636261#,
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
                       14 => 16#00000018#,
                       15 => 16#00000000#),
      DXPL_DIGEST  => (0  => 16#CBBD544E#,
                       1  => 16#A4B8B5BB#,
                       2  => 16#1129116C#,
                       3  => 16#51F42FC7#,
                       4  => 16#89B76053#,
                       5  => 16#07DE60B7#,
                       6  => 16#1D030F84#,
                       7  => 16#6097891B#,
                       8  => 16#5DF66003#,
                       9  => 16#2E15C297#,
                       10 => 16#D4E3EBA6#,
                       11 => 16#8349BF62#,
                       12 => 16#675F2D1E#,
                       13 => 16#920914B6#,
                       14 => 16#D8850532#,
                       15 => 16#CE71D29F#));

   -- Input Message: "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#64636261#,
                       1  => 16#65646362#,
                       2  => 16#66656463#,
                       3  => 16#67666564#,
                       4  => 16#68676665#,
                       5  => 16#69686766#,
                       6  => 16#6A696867#,
                       7  => 16#6B6A6968#,
                       8  => 16#6C6B6A69#,
                       9  => 16#6D6C6B6A#,
                       10 => 16#6E6D6C6B#,
                       11 => 16#6F6E6D6C#,
                       12 => 16#706F6E6D#,
                       13 => 16#71706F6E#,
                       14 => 16#00000080#,
                       15 => 16#00000000#),
      DXPL_DIGEST  => (0  => 16#153AF0A2#,
                       1  => 16#A206052B#,
                       2  => 16#7FD6CC08#,
                       3  => 16#DE0D7857#,
                       4  => 16#7FC60E49#,
                       5  => 16#1B4081A7#,
                       6  => 16#09AEE1DD#,
                       7  => 16#D56ACA77#,
                       8  => 16#2A1012E0#,
                       9  => 16#2530AED5#,
                       10 => 16#97DB99E3#,
                       11 => 16#35DDFDFF#,
                       12 => 16#AED865CE#,
                       13 => 16#E25EFB97#,
                       14 => 16#8BD01ABB#,
                       15 => 16#A9DC7C97#));
end EdonR256;
