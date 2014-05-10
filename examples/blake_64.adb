--!  BLAKE_64.adb
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
--!  Description of 'BLAKE-32' with old rotation constants.
------------------------------------------------------------------------
procedure BLAKE_64 is

   -------------------------------------------
   --  Constants for BLAKE-64 and BLAKE-48  --
   -------------------------------------------

   c64 : DXPL_Types_64.Word_Array_16 :=
     (16#243F6A8885A308D3#,
      16#13198A2E03707344#,
      16#A4093822299F31D0#,
      16#082EFA98EC4E6C89#,
      16#452821E638D01377#,
      16#BE5466CF34E90C6C#,
      16#C0AC29B7C97C50DD#,
      16#3F84D5B5B5470917#,
      16#9216D5D98979FB1B#,
      16#D1310BA698DFB5AC#,
      16#2FFD72DBD01ADFB7#,
      16#B8E1AFED6A267E96#,
      16#BA7C9045F12C7F99#,
      16#24A19947B3916CF7#,
      16#0801F2E2858EFC16#,
      16#636920D871574E69#);

   -------------------------------------------
   --  Initial values ( IV64 for BLAKE-64)  --
   -------------------------------------------

   IV64 : DXPL_Types_64.Word_Array_8 :=
     (16#6A09E667F3BCC908#,
      16#BB67AE8584CAA73B#,
      16#3C6EF372FE94F82B#,
      16#A54FF53A5F1D36F1#,
      16#510E527FADE682D1#,
      16#9B05688C2B3E6C1F#,
      16#1F83D9ABFB41BD6B#,
      16#5BE0CD19137E2179#);

   ----------------------------------------
   --  The 10 permutations of (0,..,15}  --
   ----------------------------------------

   subtype Permute_Positive is Natural range 0 .. 15;
   type TwoD_Array is array (0 .. 19, 0 .. 15) of Permute_Positive;
   SIGMA : TwoD_Array :=
     (( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15),
      (14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3),
      (11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4),
      ( 7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8),
      ( 9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13),
      ( 2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9),
      (12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11),
      (13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10),
      ( 6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5),
      (10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0), 
      ( 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15),
      (14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3),
      (11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4),
      ( 7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8),
      ( 9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13),
      ( 2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9),
      (12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11),
      (13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10),
      ( 6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5),
      (10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0));

   ----------------------------
   --  Compression Function  --
   ----------------------------

   M : DXPL_Types_64.Word_Array_16;

   procedure G64 (A, B, C, D, I : in Integer; Message : in out Word_Array_16) is
   begin
     --# BEGIN
     Message(A) := (Message(A) + Message(B)) +
                   (M(SIGMA(DXPL_Types_64.Rounds - 1, 2*I)) xor C64 (SIGMA (DXPL_Types_64.Rounds - 1, 2*I+1)));
     Message(D) := DXPL_Types_64.Rotate_Right (Message(D) xor Message(A), 32);
     Message(C) := Message(C) + Message(D);
     Message(B) := DXPL_Types_64.Rotate_Right (Message(B) xor Message(C), 25);
     Message(A) := (Message(A) + Message(B)) +
                   (M(SIGMA(DXPL_Types_64.Rounds - 1, 2*I+1)) xor C64 (SIGMA (DXPL_Types_64.Rounds - 1, 2*I)));
     Message(D) := DXPL_Types_64.Rotate_Right (Message(D) xor Message(A), 16);
     Message(C) := (Message(C) + Message(D));
     Message(B) := DXPL_Types_64.Rotate_Right (Message(B) xor Message(C), 11);
     --# END
   end G64;

   ----------------------------------
   --  Initialization of BLAKE-64  --
   ----------------------------------

   procedure DXPL_Initialize (Message : in out DXPL_Types_64.Word_Array_16) is
   begin
     --  store the original message
     M(0 .. 15) := Message (0 .. 15);

     Message( 0) := IV64(0);
     Message( 1) := IV64(1);
     Message( 2) := IV64(2);
     Message( 3) := IV64(3);
     Message( 4) := IV64(4);
     Message( 5) := IV64(5);
     Message( 6) := IV64(6);
     Message( 7) := IV64(7);
     -- removed the salt
     --  Message( 8) := salt32 (0) xor c64(0);
     --  Message( 9) := salt32 (1) xor c64(1);
     --  Message(10) := salt32 (2) xor c64(2);
     --  Message(11) := salt32 (3) xor c64(3);
     Message( 8) := c64(0);
     Message( 9) := c64(1);
     Message(10) := c64(2);
     Message(11) := c64(3);
     -- if special case t=0 for the last block
     Message(12) := c64(4);
     Message(13) := c64(5);
     Message(14) := c64(6);
     Message(15) := c64(7);
     -- else
     --  Message(12) := t32(0) xor c64(4);
     --  Message(13) := t32(0) xor c64(5);
     --  Message(14) := t32(1) xor c64(6);
     --  Message(15) := t32(1) xor c64(7);
   end DXPL_Initialize;

   -------------------------------------
   --  Process one round of BLAKE-64  --
   -------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_64.Word_Array_16) is
   begin
     --  column step
     G64 (0, 4,  8, 12, 0, Message);
     G64 (1, 5,  9, 13, 1, Message);
     G64 (2, 6, 10, 14, 2, Message);
     G64 (3, 7, 11, 15, 3, Message);    

     --  diagonal step
     G64 (0, 5, 10, 15, 4, Message);
     G64 (1, 6, 11, 12, 5, Message);
     G64 (2, 7,  8, 13, 6, Message);
     G64 (3, 4,  9, 14, 7, Message);
   
   --------------------
   --  Finalization  --
   --------------------

   procedure DXPL_Finalize (Message : in out DXPL_Types_64.Word_Array_16) is
   begin
     --  This code is not needed for this analyzation, because its
     --  overrides the initial values for a multi-block message
     --  IV64(0) := IV64(0) xor Message( 0) xor Message( 8); -- omitted xor salt32(0);
     --  IV64(1) := IV64(1) xor Message( 1) xor Message( 9); -- omitted xor salt32(1);
     --  IV64(2) := IV64(2) xor Message( 2) xor Message(10); -- omitted xor salt32(2);
     --  IV64(3) := IV64(3) xor Message( 3) xor Message(11); -- omitted xor salt32(3);
     --  IV64(4) := IV64(4) xor Message( 4) xor Message(12); -- omitted xor salt32(0);
     --  IV64(5) := IV64(5) xor Message( 5) xor Message(13); -- omitted xor salt32(1);
     --  IV64(6) := IV64(6) xor Message( 6) xor Message(14); -- omitted xor salt32(2);
     --  IV64(7) := IV64(7) xor Message( 7) xor Message(15); -- omitted xor salt32(3);
     null;
   end DXPL_Finalize;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "BLAKE-64",
      DXPL_ROUNDS      => 14,
      DXPL_TERMINATION => 512);

   --  Input Message ''
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#0000000000800000#,
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
                       13 => 16#0000000000000001#,
                       14 => 16#0000000000000000#,
                       15 => 16#0000000000000008#),
      DXPL_DIGEST  => (0  => 16#11EEE5D2292D069E#,
                       1  => 16#990E2730FB70D75E#,
                       2  => 16#50772CD4E8BA4EFB#,
                       3  => 16#1ED82F8C8C81B9FB#,
                       4  => 16#CB99A943981927A8#,
                       5  => 16#2BAABF7196728C83#,
                       6  => 16#3E9E34E12A80D561#,
                       7  => 16#576662533A62F81E#,
                       8  => 16#8091032A75884AC8#,
                       9  => 16#C09C051E14EF6F3D#,
                       10 => 16#C1E159470A02DA89#,
                       11 => 16#2F816EBB664BF6D7#,
                       12 => 16#9C4D13A88DAFF9DE#,
                       13 => 16#7101C972D53D2194#,
                       14 => 16#6750CCB5B0E68CD3#,
                       15 => 16#7BC68226C14AEE8A#));

   --  Input Message: "abc"
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
                       14 => 16#0000000000000018#,
                       15 => 16#0000000000000000#),
      DXPL_DIGEST  => (0  => 16#F29092787EB58903#,
                       1  => 16#D2FDE7EB05D1E778#,
                       2  => 16#B9A82F18120DF533#,
                       3  => 16#51A37B9C63BDBFE5#,
                       4  => 16#2B6AF9F1D8C10774#,
                       5  => 16#5E7EABD8783CC4F7#,
                       6  => 16#169C8E8DC970345C#,
                       7  => 16#F9EFED7C68974A9E#,
                       8  => 16#BF2785CF9A014576#,
                       9  => 16#C7240A69F2FEDD16#,
                       10 => 16#B94CE0F4B7D7EFBC#,
                       11 => 16#316C049363BEBAE6#,
                       12 => 16#86F0FEAD16B22563#,
                       13 => 16#B6E30184C3E25AB0#,
                       14 => 16#24B69FF9C6A8032F#,
                       15 => 16#C245DF827563D56F#));
end BLAKE_64;

