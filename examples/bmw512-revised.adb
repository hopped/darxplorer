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
--!  Description of 'Blue Midnight Wish (BMW) 512'. It is based on
--!  a tweaked subscription to NIST for the second round.
------------------------------------------------------------------------
procedure BMW512_Revised is

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
      H     : in DXPL_Types_32.Word_Array_16;
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
              (DXPL_Types_64.Word (Index) * 16#0555555555555555# +
               DXPL_Types_32.Rotate_Left (X ((Index - 16) mod 16), ((Index - 16) mod 16) + 1) +
               DXPL_Types_32.Rotate_Left (X ((Index - 13) mod 16), ((Index - 13) mod 16) + 1) -
               DXPL_Types_32.Rotate_Left (X ((Index -  6) mod 16), ((Index -  6) mod 16) + 1) xor
               H ((Index - 16 + 7) mod 16)));
   end ExpansionRound_1;

   function ExpansionRound_2
   -- Message expansion function 2
     (Index : in Integer;
      X     : in DXPL_Types_64.Word_Array_16;
      H     : in DXPL_Types_32.Word_Array_16;
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
              s64s (Q (Index - 2), 1) +
              s64s (Q (Index - 1), 2) +
              (DXPL_Types_64.Word (Index) * 16#0555555555555555# +
               DXPL_Types_32.Rotate_Left (X ((Index - 16) mod 16), ((Index - 16) mod 16) + 1) +
               DXPL_Types_32.Rotate_Left (X ((Index - 13) mod 16), ((Index - 13) mod 16) + 1) -
               DXPL_Types_32.Rotate_Left (X ((Index -  6) mod 16), ((Index -  6) mod 16) + 1) xor
               H ((Index - 16 + 7) mod 16)));
   end ExpansionRound_2;

   procedure DXPL_Initialize (H : in out DXPL_Types_64.Word_Array_16) is
   begin
      H := IV;
   end DXPL_Initialize;

   -----------------------------------------------------------
   -- Process a compression block of Blue Midnight Wish 512 --
   -----------------------------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_16; H : in out DXPL_Types_64.Word_Array_16) is
   begin
      --# BEGIN
      --  This part is the function f0 - in the documentation
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

      --  Diffuse the differences in every word in a bijective manner with s32c,
      --  and then add the values of the previous double pipe.
      Q (0)  := s64c (W ( 0), S (0)) + H ( 1);
      Q (1)  := s64c (W ( 1), S (1)) + H ( 2);
      Q (2)  := s64c (W ( 2), S (2)) + H ( 3);
      Q (3)  := s64c (W ( 3), S (3)) + H ( 4);
      Q (4)  := s64s (W ( 4), 1)     + H ( 5);
      Q (5)  := s64c (W ( 5), S (0)) + H ( 6);
      Q (6)  := s64c (W ( 6), S (1)) + H ( 7);
      Q (7)  := s64c (W ( 7), S (2)) + H ( 8);
      Q (8)  := s64c (W ( 8), S (3)) + H ( 9);
      Q (9)  := s64s (W ( 9), 1)     + H (10);
      Q (10) := s64c (W (10), S (0)) + H (11);
      Q (11) := s64c (W (11), S (1)) + H (12);
      Q (12) := s64c (W (12), S (2)) + H (13);
      Q (13) := s64c (W (13), S (3)) + H (14);
      Q (14) := s64s (W (14), 1)     + H (15);
      Q (15) := s64c (W (15), S (0)) + H ( 0);

      --  This is the Message expansion or f_1 in the documentation.
      --  It has 16 rounds.
      --  Blue Midnight Wish has two tunable security parameters.
      --  The parameters are named EXPAND_1_ROUNDS and EXPAND_2_ROUNDS.
      --  The following relation for these parameters should is satisfied:
      --  EXPAND_1_ROUNDS + EXPAND_2_ROUNDS = 16
      for Index in 0 .. 1 loop
         Q (Index + 16) := ExpansionRound_1 (Index + 16, Message, H, Q);
      end loop;

      for Index in 2 .. 15 loop
         Q (Index + 16) := ExpansionRound_2 (Index + 16, Message, H, Q);
      end loop;

      declare
         --  Blue Midnight Wish has two temporary cummulative variables that accumulate via XORing
         --  16 new variables that are prooduced in the Message Expansion part.
         XL64 : DXPL_Types_64.Word := Q (16) xor Q (17) xor Q (18) xor Q (19) xor Q (20) xor Q (21) xor Q (22) xor Q (23);
         XH64 : DXPL_Types_64.Word := XL64   xor Q (24) xor Q (25) xor Q (26) xor Q (27) xor Q (28) xor Q (29) xor Q (30) xor Q (31);
      begin
         --  This part is the function f_2 - in the documentation
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
      DXPL_DIGEST  => (0  => 16#116F1A73854A98C4#,
                       1  => 16#A3BC92227B5EDA0A#,
                       2  => 16#18571783B50C23B4#,
                       3  => 16#52EA0825D1897A15#,
                       4  => 16#192356EE9D50EB83#,
                       5  => 16#695D5F05A5E83053#,
                       6  => 16#B0B9DE1724BC8A9A#,
                       7  => 16#B399EC00F879513D#,
                       8  => 16#253EE9A47CBF875A#,
                       9  => 16#E4AD1E7D851B395D#,
                       10 => 16#1488BDDA9C3FB702#,
                       11 => 16#8B5BBD3DE80AE0B2#,
                       12 => 16#DDE3374BE6513EDB#,
                       13 => 16#5EA9F61878AF30DB#,
                       14 => 16#41DAA9BFD2B84278#,
                       15 => 16#3DFD261C6F242BA3#));

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
      DXPL_DIGEST  => (0  => 16#464E9CFD277A05F8#,
                       1  => 16#C4C5C30397FB071F#,
                       2  => 16#D5ED9C3BDCC48F99#,
                       3  => 16#B57DB0FABA7CA23E#,
                       4  => 16#D1B3A76FDB7661C5#,
                       5  => 16#B095CFEBCA35A87F#,
                       6  => 16#AB8570E984F44E0A#,
                       7  => 16#0448A7C5BCBB8091#,
                       8  => 16#311866A903E94306#,
                       9  => 16#E49F3725C0510562#,
                       10 => 16#CAE393A3EA90F276#,
                       11 => 16#66EE562EF9C1FE08#,
                       12 => 16#B1D387F1225D6FEB#,
                       13 => 16#206204E5852DB655#,
                       14 => 16#FD0FE0AAB5B59052#,
                       15 => 16#DC819B273B83B3BF#));
end BMW512_Revised;
