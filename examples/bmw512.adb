--!  bmw512.adb
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
with DXPL_Types_64; use DXPL_Types_64;

------------------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 09/04/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of 'Blue Midnight Wish (BMW) 512'. It is based on the
--!  first subscription to NIST.
------------------------------------------------------------------------
procedure BMW512 is

   --------------------------------
   -- Additional type definition --
   --------------------------------

   W : DXPL_Types_64.Word_Array_16 := (others => 2#0#);
   Q : DXPL_Types_64.Word_Array_32 := (others => 2#0#);

   ----------------------------------------------
   -- Initial double chaining pipe of BMW-512 --
   ----------------------------------------------

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

   ----------------------------------------------------------------------------------------
   -- Constants used for rotation/shift operations applied in the logic functions 's64c' --
   ----------------------------------------------------------------------------------------

   type Constants is array (0 .. 3) of Integer;
   type Const_Array is array (0 .. 3) of Constants;
   S : Const_Array := 
     (0 => (1, 3, 4, 37),
      1 => (1, 2, 13, 43),
      2 => (2, 1, 19, 53),
      3 => (2, 2, 28, 59));

   -------------------------
   -- Additional functins --
   --------------------------

   function s64c (X : in DXPL_Types_64.Word; C : in Constants) return DXPL_Types_64.Word is
   -- Logic functions (complex)
   begin
      return DXPL_Types_64.Shift_Right (X, C (0)) xor
             DXPL_Types_64.Shift_Left  (X, C (1)) xor
             DXPL_Types_64.Rotate_Left (X, C (2)) xor
             DXPL_Types_64.Rotate_Left (X, C (3));
   end s64c;

   function s64s (X : in DXPL_Types_64.Word; I : in Integer) return DXPL_Types_64.Word is
   -- Logic functions (simple)
   begin
      return DXPL_Types_64.Shift_Right (X, I) xor X;
   end s64s;

   function ExpansionRound_1
   -- Message expansion function 1
     (Index : in Integer;
      X     : in DXPL_Types_64.Word_Array_16;
      Q     : in DXPL_Types_64.Word_Array_32)
      return  DXPL_Types_64.Word is
   begin
      return (s64c (Q (Index - 16), S (1)) +
              s64c (Q (Index - 15), S (2)) +
              s64c (Q (Index - 14), S (3)) +
              s64c (Q (Index - 13), S (0)) +
              s64c (Q (Index - 12), S (1)) +
              s64c (Q (Index - 11), S (2)) +
              s64c (Q (Index - 10), S (3)) +
              s64c (Q (Index -  9), S (0)) +
              s64c (Q (Index -  8), S (1)) +
              s64c (Q (Index -  7), S (2)) +
              s64c (Q (Index -  6), S (3)) +
              s64c (Q (Index -  5), S (0)) +
              s64c (Q (Index -  4), S (1)) +
              s64c (Q (Index -  3), S (2)) +
              s64c (Q (Index -  2), S (3)) +
              s64c (Q (Index -  1), S (0)) +
              DXPL_Types_64.Word (Index) * 16#0555555555555555# +
              X ((Index - 16) mod 16) +
              X ((Index - 13) mod 16) -
              X ((Index -  6) mod 16));
   end ExpansionRound_1;

   function ExpansionRound_2
   -- Message expansion function 2
     (Index : in Integer;
      X     : in DXPL_Types_64.Word_Array_16;
      Q     : in DXPL_Types_64.Word_Array_32)
      return  DXPL_Types_64.Word is
   begin
      return ((Q (Index - 16)) + DXPL_Types_64.Rotate_Left (Q (Index - 15),  5) +
              (Q (Index - 14)) + DXPL_Types_64.Rotate_Left (Q (Index - 13), 11) +
              (Q (Index - 12)) + DXPL_Types_64.Rotate_Left (Q (Index - 11), 27) +
              (Q (Index - 10)) + DXPL_Types_64.Rotate_Left (Q (Index -  9), 32) +
              (Q (Index -  8)) + DXPL_Types_64.Rotate_Left (Q (Index -  7), 37) +
              (Q (Index -  6)) + DXPL_Types_64.Rotate_Left (Q (Index -  5), 43) +
              (Q (Index -  4)) + DXPL_Types_64.Rotate_Left (Q (Index -  3), 53) +
              s64s (Q (Index - 2), 2) +
              s64s (Q (Index - 1), 1) +
              DXPL_Types_64.Word (Index) * 16#0555555555555555# +
              X ((Index - 16) mod 16) +
              X ((Index - 13) mod 16) -
              X ((Index -  6) mod 16));
   end ExpansionRound_2;

   procedure DXPL_Initialize (H : in out DXPL_Types_64.Word_Array_16) is
   begin
      H := IV;
   end DXPL_Initialize;

   -----------------------------------------------------------
   -- Process a compression block of Blue Midnight Wish 512 --
   -----------------------------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_16; H : in out DXPL_Types_64.Word_Array_16) is
   --  Function F_0 and function F_1
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

      Q (0)  := s64c (W  (0), S (0));
      Q (1)  := s64c (W  (1), S (1));
      Q (2)  := s64c (W  (2), S (2));
      Q (3)  := s64c (W  (3), S (3));
      Q (4)  := s64s (W  (4), 1);
      Q (5)  := s64c (W  (5), S (0));
      Q (6)  := s64c (W  (6), S (1));
      Q (7)  := s64c (W  (7), S (2));
      Q (8)  := s64c (W  (8), S (3));
      Q (9)  := s64s (W  (9), 1);
      Q (10) := s64c (W (10), S (0));
      Q (11) := s64c (W (11), S (1));
      Q (12) := s64c (W (12), S (2));
      Q (13) := s64c (W (13), S (3));
      Q (14) := s64s (W (14), 1);
      Q (15) := s64c (W (15), S (0));

      for Index in 0 .. 1 loop
         Q (Index + 16) := ExpansionRound_1 (Index + 16, Message, Q);
      end loop;

      for Index in 2 .. 15 loop
         Q (Index + 16) := ExpansionRound_2 (Index + 16, Message, Q);
      end loop;

      declare
         XL64 : DXPL_Types_64.Word := Q (16) xor Q (17) xor Q (18) xor Q (19) xor Q (20) xor Q (21) xor Q (22) xor Q (23);
         XH64 : DXPL_Types_64.Word := XL64   xor Q (24) xor Q (25) xor Q (26) xor Q (27) xor Q (28) xor Q (29) xor Q (30) xor Q (31);
      begin
         H (0) := (DXPL_Types_64.Shift_Left  (XH64,  5) xor DXPL_Types_64.Shift_Right (Q (16), 5) xor Message (0)) + (XL64 xor Q (24) xor Q (0));
         H (1) := (DXPL_Types_64.Shift_Right (XH64,  7) xor DXPL_Types_64.Shift_Left  (Q (17), 8) xor Message (1)) + (XL64 xor Q (25) xor Q (1));
         H (2) := (DXPL_Types_64.Shift_Right (XH64,  5) xor DXPL_Types_64.Shift_Left  (Q (18), 5) xor Message (2)) + (XL64 xor Q (26) xor Q (2));
         H (3) := (DXPL_Types_64.Shift_Right (XH64,  1) xor DXPL_Types_64.Shift_Left  (Q (19), 5) xor Message (3)) + (XL64 xor Q (27) xor Q (3));
         H (4) := (DXPL_Types_64.Shift_Right (XH64,  3) xor Q (20) xor Message (4)) + (XL64 xor Q (28) xor Q (4));
         H (5) := (DXPL_Types_64.Shift_Left  (XH64,  6) xor DXPL_Types_64.Shift_Right (Q (21), 6) xor Message (5)) + (XL64 xor Q (29) xor Q (5));
         H (6) := (DXPL_Types_64.Shift_Right (XH64,  4) xor DXPL_Types_64.Shift_Left  (Q (22), 6) xor Message (6)) + (XL64 xor Q (30) xor Q (6));
         H (7) := (DXPL_Types_64.Shift_Right (XH64, 11) xor DXPL_Types_64.Shift_Left  (Q (23), 2) xor Message (7)) + (XL64 xor Q (31) xor Q (7));

         H (8)  := DXPL_Types_64.Rotate_Left (H (4),  9) + (XH64 xor Q (24) xor Message  (8)) + (DXPL_Types_64.Shift_Left  (XL64, 8) xor Q (23) xor Q  (8));
         H (9)  := DXPL_Types_64.Rotate_Left (H (5), 10) + (XH64 xor Q (25) xor Message  (9)) + (DXPL_Types_64.Shift_Right (XL64, 6) xor Q (16) xor Q  (9));
         H (10) := DXPL_Types_64.Rotate_Left (H (6), 11) + (XH64 xor Q (26) xor Message (10)) + (DXPL_Types_64.Shift_Left  (XL64, 6) xor Q (17) xor Q (10));
         H (11) := DXPL_Types_64.Rotate_Left (H (7), 12) + (XH64 xor Q (27) xor Message (11)) + (DXPL_Types_64.Shift_Left  (XL64, 4) xor Q (18) xor Q (11));
         H (12) := DXPL_Types_64.Rotate_Left (H (0), 13) + (XH64 xor Q (28) xor Message (12)) + (DXPL_Types_64.Shift_Right (XL64, 3) xor Q (19) xor Q (12));
         H (13) := DXPL_Types_64.Rotate_Left (H (1), 14) + (XH64 xor Q (29) xor Message (13)) + (DXPL_Types_64.Shift_Right (XL64, 4) xor Q (20) xor Q (13));
         H (14) := DXPL_Types_64.Rotate_Left (H (2), 15) + (XH64 xor Q (30) xor Message (14)) + (DXPL_Types_64.Shift_Right (XL64, 7) xor Q (21) xor Q (14));
         H (15) := DXPL_Types_64.Rotate_Left (H (3), 16) + (XH64 xor Q (31) xor Message (15)) + (DXPL_Types_64.Shift_Right (XL64, 2) xor Q (22) xor Q (15));
      end;
      --# END
   end DXPL_Process;

   procedure DXPL_Finalize (Message : in out DXPL_Types_64.Word_Array_16; H : in out DXPL_Types_64.Word_Array_16) is
   begin
      Message := H;
   end DXPL_Finalize;

   -------------
   --  SETUP  --
   -------------

begin
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "BMW-512",
      DXPL_ROUNDS      => 1,
      DXPL_TERMINATION => 512);

   -- Input Message is 'abc'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#80636261#,
                       1  => 16#0#,
                       2  => 16#0#,
                       3  => 16#0#,
                       4  => 16#0#,
                       5  => 16#0#,
                       6  => 16#0#,
                       7  => 16#0#,
                       8  => 16#0#,
                       9  => 16#0#,
                       10 => 16#0#,
                       11 => 16#0#,
                       12 => 16#0#,
                       13 => 16#0#,
                       14 => 16#00000018#,
                       15 => 16#0#),
      DXPL_DIGEST  => (0  => 16#EBA7F05891A589FF#,
                       1  => 16#FD8998AC25B68BB1#,
                       2  => 16#1D36E282DF7CDB96#,
                       3  => 16#4A04C725977F3E57#,
                       4  => 16#FAB20C1791F25F2F#,
                       5  => 16#AB7282302DAA986A#,
                       6  => 16#BC2739109F367FBB#,
                       7  => 16#1AEA856C2D808A8F#,
                       8  => 16#27342D529235D8DE#,
                       9  => 16#2059753338B35712#,
                       10 => 16#666E9ABD4DBC4949#,
                       11 => 16#E8BA735657938314#,
                       12 => 16#73C23F3A6AB73473#,
                       13 => 16#8AAC854907F33841#,
                       14 => 16#D71CA30CCE71FFF8#,
                       15 => 16#CF6D934908F35D64#));

   -- Input Message is 'bcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijk'
   --                  'lmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnmnopqrsmopqrstu'
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

      DXPL_DIGEST  => (0  => 16#C09684C82A23F1EF#,
                       1  => 16#7AD0F332E26CCC84#,
                       2  => 16#83B150CBA7D67974#,
                       3  => 16#B42D185FC7762579#,
                       4  => 16#448202BF2F4DE650#,
                       5  => 16#CB43EFF1449CD572#,
                       6  => 16#F236095F94F13142#,
                       7  => 16#9BDEBDEA3096189F#,
                       8  => 16#C4AEE4B3E729B175#,
                       9  => 16#C1FD695F1617C252#,
                       10 => 16#175AC24FB486D883#,
                       11 => 16#8B2022581FFD67E4#,
                       12 => 16#956D141DD093C8AB#,
                       13 => 16#8F3DD5A788F2341F#,
                       14 => 16#FF851BE447FBDF04#,
                       15 => 16#77C6F4E2C2A08A12#));
end BMW512;
