--!  bmw256.adb
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
--!  Description of 'Blue Midnight Wish (BMW) 256'. It is based on
--!  the first subscription to NIST.
------------------------------------------------------------------------
procedure BMW256 is

   -----------------------------------
   --  Additional type definitions  --
   -----------------------------------

   W : DXPL_Types_32.Word_Array_16 := (others => 2#0#);
   Q : DXPL_Types_32.Word_Array_32 := (others => 2#0#);

   -----------------------------------------------
   --  Initial double chaining pipe of BMW-256  --
   -----------------------------------------------

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

   --------------------------------------------------------
   --  Constants used for rotation and shift operations  --
   --  applied in the logic function 's32c'              --
   --------------------------------------------------------

   type Constants   is array (0 .. 3) of Integer;
   type Const_Array is array (0 .. 3) of Constants;
   S : Const_Array := 
     (0 => (1, 3,  4, 19),
      1 => (1, 2,  8, 23),
      2 => (2, 1, 12, 25),
      3 => (2, 2, 15, 29));

   ----------------------------
   --  Additional functions  --
   ----------------------------

   function s32c (X : in DXPL_Types_32.Word; C : in Constants) return DXPL_Types_32.Word is
   --  complex 'logic' function
   begin
      return DXPL_Types_32.Shift_Right (X, C (0)) xor
             DXPL_Types_32.Shift_Left  (X, C (1)) xor
             DXPL_Types_32.Rotate_Left (X, C (2)) xor 
             DXPL_Types_32.Rotate_Left (X, C (3));
   end s32c;

   function s32s (X : in DXPL_Types_32.Word; I : in Integer) return DXPL_Types_32.Word is
   --  simple 'logic' function
   begin
      return DXPL_Types_32.Shift_Right (X, I) xor X;
   end s32s;

   function ExpansionRound_1
   --  Expansion function 1
     (Index : in Integer;
      X     : in DXPL_Types_32.Word_Array_16;
      Q     : in DXPL_Types_32.Word_Array_32)
      return  DXPL_Types_32.Word is
   begin
      return (s32c (Q (Index - 16), S (1)) +
              s32c (Q (Index - 15), S (2)) +
              s32c (Q (Index - 14), S (3)) +
              s32c (Q (Index - 13), S (0)) +
              s32c (Q (Index - 12), S (1)) +
              s32c (Q (Index - 11), S (2)) +
              s32c (Q (Index - 10), S (3)) +
              s32c (Q (Index -  9), S (0)) +
              s32c (Q (Index -  8), S (1)) +
              s32c (Q (Index -  7), S (2)) +
              s32c (Q (Index -  6), S (3)) +
              s32c (Q (Index -  5), S (0)) +
              s32c (Q (Index -  4), S (1)) +
              s32c (Q (Index -  3), S (2)) +
              s32c (Q (Index -  2), S (3)) +
              s32c (Q (Index -  1), S (0)) +
              DXPL_Types_32.Word (Index) * 16#05555555# +
              X ((Index - 16) mod 16) +
              X ((Index - 13) mod 16) -
              X ((Index -  6) mod 16));
   end ExpansionRound_1;

   function ExpansionRound_2
   --  Expansion function 2
     (Index : in Integer;
      X     : in DXPL_Types_32.Word_Array_16;
      Q     : in DXPL_Types_32.Word_Array_32)
      return  DXPL_Types_32.Word is
   begin
      return ((Q (Index - 16)) + DXPL_Types_32.Rotate_Left (Q (Index - 15),  3) +
              (Q (Index - 14)) + DXPL_Types_32.Rotate_Left (Q (Index - 13),  7) +
              (Q (Index - 12)) + DXPL_Types_32.Rotate_Left (Q (Index - 11), 13) +
              (Q (Index - 10)) + DXPL_Types_32.Rotate_Left (Q (Index -  9), 16) +
              (Q (Index -  8)) + DXPL_Types_32.Rotate_Left (Q (Index -  7), 19) +
              (Q (Index -  6)) + DXPL_Types_32.Rotate_Left (Q (Index -  5), 23) +
              (Q (Index -  4)) + DXPL_Types_32.Rotate_Left (Q (Index -  3), 27) +
              s32s (Q (Index - 2), 2) + 
              s32s (Q (Index - 1), 1) + DXPL_Types_32.Word (Index) * 16#05555555# +
              X ((Index - 16) mod 16) +
              X ((Index - 13) mod 16) -
              X ((Index -  6) mod 16));
   end ExpansionRound_2;

   procedure DXPL_Initialize (H : in out DXPL_Types_32.Word_Array_16) is
   begin
      H := IV;
   end DXPL_Initialize;

   -------------------------------------------------------------
   --  Process a compression block of Blue Midnight Wish 256  --
   -------------------------------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_32.Word_Array_16; H : in out DXPL_Types_32.Word_Array_16) is
   -- FUNCTION F_0 followed by FUNCTION F_1
   begin
      --# BEGIN
      W (0)  := (Message  (5) xor H  (5)) - (Message (7) xor H (7)) + (Message (10) xor H (10)) + (Message (13) xor H (13)) + (Message (14) xor H (14));
      W (1)  := (Message  (6) xor H  (6)) - (Message (8) xor H (8)) + (Message (11) xor H (11)) + (Message (14) xor H (14)) - (Message (15) xor H (15));
      W (2)  := (Message  (0) xor H  (0)) + (Message (7) xor H (7)) + (Message  (9) xor H  (9)) - (Message (12) xor H (12)) + (Message (15) xor H (15));
      W (3)  := (Message  (0) xor H  (0)) - (Message (1) xor H (1)) + (Message  (8) xor H  (8)) - (Message (10) xor H (10)) + (Message (13) xor H (13));
      W (4)  := (Message  (1) xor H  (1)) + (Message (2) xor H (2)) + (Message  (9) xor H  (9)) - (Message (11) xor H (11)) - (Message (14) xor H (14));
      W (5)  := (Message  (3) xor H  (3)) - (Message (2) xor H (2)) + (Message (10) xor H (10)) - (Message (12) xor H (12)) + (Message (15) xor H (15));
      W (6)  := (Message  (4) xor H  (4)) - (Message (0) xor H (0)) - (Message  (3) xor H  (3)) - (Message (11) xor H (11)) + (Message (13) xor H (13));
      W (7)  := (Message  (1) xor H  (1)) - (Message (4) xor H (4)) - (Message  (5) xor H  (5)) - (Message (12) xor H (12)) - (Message (14) xor H (14));
      W (8)  := (Message  (2) xor H  (2)) - (Message (5) xor H (5)) - (Message  (6) xor H  (6)) + (Message (13) xor H (13)) - (Message (15) xor H (15));
      W (9)  := (Message  (0) xor H  (0)) - (Message (3) xor H (3)) + (Message  (6) xor H  (6)) - (Message  (7) xor H  (7)) + (Message (14) xor H (14));
      W (10) := (Message  (8) xor H  (8)) - (Message (1) xor H (1)) - (Message  (4) xor H  (4)) - (Message  (7) xor H  (7)) + (Message (15) xor H (15));
      W (11) := (Message  (8) xor H  (8)) - (Message (0) xor H (0)) - (Message  (2) xor H  (2)) - (Message  (5) xor H  (5)) + (Message  (9) xor H  (9));
      W (12) := (Message  (1) xor H  (1)) + (Message (3) xor H (3)) - (Message  (6) xor H  (6)) - (Message  (9) xor H  (9)) + (Message (10) xor H (10));
      W (13) := (Message  (2) xor H  (2)) + (Message (4) xor H (4)) + (Message  (7) xor H  (7)) + (Message (10) xor H (10)) + (Message (11) xor H (11));
      W (14) := (Message  (3) xor H  (3)) - (Message (5) xor H (5)) + (Message  (8) xor H  (8)) - (Message (11) xor H (11)) - (Message (12) xor H (12));
      W (15) := (Message (12) xor H (12)) - (Message (4) xor H (4)) - (Message  (6) xor H  (6)) - (Message  (9) xor H  (9)) + (Message (13) xor H (13));

      Q (0)  := s32c (W (0), S (0));
      Q (1)  := s32c (W (1), S (1));
      Q (2)  := s32c (W (2), S (2));
      Q (3)  := s32c (W (3), S (3));
      Q (4)  := s32s (W (4), 1);
      Q (5)  := s32c (W (5), S (0));
      Q (6)  := s32c (W (6), S (1));
      Q (7)  := s32c (W (7), S (2));
      Q (8)  := s32c (W (8), S (3));
      Q (9)  := s32s (W (9), 1);
      Q (10) := s32c (W (10), S (0));
      Q (11) := s32c (W (11), S (1));
      Q (12) := s32c (W (12), S (2));
      Q (13) := s32c (W (13), S (3));
      Q (14) := s32s (W (14), 1);
      Q (15) := s32c (W (15), S (0));

      for Index in 0 .. 1 loop
         Q (Index + 16) := ExpansionRound_1 (Index + 16, Message, Q);
      end loop;

      for Index in 2 .. 15 loop
         Q (Index + 16) := ExpansionRound_2 (Index + 16, Message, Q);
      end loop;

      declare
         XL32 : DXPL_Types_32.Word := Q (16) xor Q (17) xor Q (18) xor Q (19) xor Q (20) xor Q (21) xor Q (22) xor Q (23);
         XH32 : DXPL_Types_32.Word := XL32   xor Q (24) xor Q (25) xor Q (26) xor Q (27) xor Q (28) xor Q (29) xor Q (30) xor Q (31);
      begin
         H (0) := (DXPL_Types_32.Shift_Left  (XH32,  5) xor DXPL_Types_32.Shift_Right (Q (16), 5) xor Message (0)) + (XL32 xor Q (24) xor Q (0));
         H (1) := (DXPL_Types_32.Shift_Right (XH32,  7) xor DXPL_Types_32.Shift_Left  (Q (17), 8) xor Message (1)) + (XL32 xor Q (25) xor Q (1));
         H (2) := (DXPL_Types_32.Shift_Right (XH32,  5) xor DXPL_Types_32.Shift_Left  (Q (18), 5) xor Message (2)) + (XL32 xor Q (26) xor Q (2));
         H (3) := (DXPL_Types_32.Shift_Right (XH32,  1) xor DXPL_Types_32.Shift_Left  (Q (19), 5) xor Message (3)) + (XL32 xor Q (27) xor Q (3));
         H (4) := (DXPL_Types_32.Shift_Right (XH32,  3) xor Q (20) xor Message (4)) + (XL32 xor Q (28) xor Q (4));
         H (5) := (DXPL_Types_32.Shift_Left  (XH32,  6) xor DXPL_Types_32.Shift_Right (Q (21), 6) xor Message (5)) + (XL32 xor Q (29) xor Q (5));
         H (6) := (DXPL_Types_32.Shift_Right (XH32,  4) xor DXPL_Types_32.Shift_Left  (Q (22), 6) xor Message (6)) + (XL32 xor Q (30) xor Q (6));
         H (7) := (DXPL_Types_32.Shift_Right (XH32, 11) xor DXPL_Types_32.Shift_Left  (Q (23), 2) xor Message (7)) + (XL32 xor Q (31) xor Q (7));

         H (8)  := DXPL_Types_32.Rotate_Left (H (4),  9) + (XH32 xor Q (24) xor Message  (8)) + (DXPL_Types_32.Shift_Left  (XL32, 8) xor Q (23) xor Q  (8));
         H (9)  := DXPL_Types_32.Rotate_Left (H (5), 10) + (XH32 xor Q (25) xor Message  (9)) + (DXPL_Types_32.Shift_Right (XL32, 6) xor Q (16) xor Q  (9));
         H (10) := DXPL_Types_32.Rotate_Left (H (6), 11) + (XH32 xor Q (26) xor Message (10)) + (DXPL_Types_32.Shift_Left  (XL32, 6) xor Q (17) xor Q (10));
         H (11) := DXPL_Types_32.Rotate_Left (H (7), 12) + (XH32 xor Q (27) xor Message (11)) + (DXPL_Types_32.Shift_Left  (XL32, 4) xor Q (18) xor Q (11));
         H (12) := DXPL_Types_32.Rotate_Left (H (0), 13) + (XH32 xor Q (28) xor Message (12)) + (DXPL_Types_32.Shift_Right (XL32, 3) xor Q (19) xor Q (12));
         H (13) := DXPL_Types_32.Rotate_Left (H (1), 14) + (XH32 xor Q (29) xor Message (13)) + (DXPL_Types_32.Shift_Right (XL32, 4) xor Q (20) xor Q (13));
         H (14) := DXPL_Types_32.Rotate_Left (H (2), 15) + (XH32 xor Q (30) xor Message (14)) + (DXPL_Types_32.Shift_Right (XL32, 7) xor Q (21) xor Q (14));
         H (15) := DXPL_Types_32.Rotate_Left (H (3), 16) + (XH32 xor Q (31) xor Message (15)) + (DXPL_Types_32.Shift_Right (XL32, 2) xor Q (22) xor Q (15));
      end;
      --# END
   end DXPL_Process;

   procedure DXPL_Finalize
     (Message : in out DXPL_Types_32.Word_Array_16;
      H       : in DXPL_Types_32.Word_Array_16) is
   begin
      Message := H;
   end DXPL_Finalize;

-----------
-- SETUP --
-----------

begin
   DXPL_Types_32.Configuration
     (DXPL_ALGORITHM   => "BMW-256",
      DXPL_ROUNDS      => 1,
      DXPL_TERMINATION => 256);

   -- Input Message: "abc"
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
      DXPL_DIGEST  => (0  => 16#FD892530#,
                       1  => 16#5EFCDD8F#,
                       2  => 16#6BCAC931#,
                       3  => 16#7E3F9EFC#,
                       4  => 16#8111BADF#,
                       5  => 16#EE80AE18#,
                       6  => 16#B65735B3#,
                       7  => 16#25DFEC61#,
                       8  => 16#E2059991#,
                       9  => 16#F5E611D1#,
                       10 => 16#5E2EB47C#,
                       11 => 16#0E24C931#,
                       12 => 16#190C67B0#,
                       13 => 16#5C8A7119#,
                       14 => 16#1FE6101B#,
                       15 => 16#254D12C5#));

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
      DXPL_DIGEST  => (0  => 16#19DFDF61#,
                       1  => 16#1FEF9581#,
                       2  => 16#6F90EB5F#,
                       3  => 16#CFE4C832#,
                       4  => 16#98EC3FAD#,
                       5  => 16#B3A01AF3#,
                       6  => 16#6B750787#,
                       7  => 16#BD43790D#,
                       8  => 16#8096B73E#,
                       9  => 16#B6625595#,
                       10 => 16#97891158#,
                       11 => 16#A46DCA7D#,
                       12 => 16#3AD9444C#,
                       13 => 16#6B221BD7#,
                       14 => 16#921A347D#,
                       15 => 16#0A11B561#));
end BMW256;
