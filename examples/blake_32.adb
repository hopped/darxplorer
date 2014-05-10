--!  blake_32.adb
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
--!  Created       :: 09/16/2009
--!  Last-Modified :: 09/16/2009
--!
--!  Description of 'BLAKE-32' with old rotation constants.
------------------------------------------------------------------------
procedure BLAKE_32 is

   -------------------------------------------
   --  Constants for BLAKE-32 and BLAKE-28  --
   -------------------------------------------

   c32 : DXPL_Types_32.Word_Array_16 :=
     (0  => 16#243F6A88#,
      1  => 16#85A308D3#,
      2  => 16#13198A2E#,
      3  => 16#03707344#,
      4  => 16#A4093822#,
      5  => 16#299F31D0#,
      6  => 16#082EFA98#,
      7  => 16#EC4E6C89#,
      8  => 16#452821E6#,
      9  => 16#38D01377#,
      10 => 16#BE5466CF#,
      11 => 16#34E90C6C#,
      12 => 16#C0AC29B7#,
      13 => 16#C97C50DD#,
      14 => 16#3F84D5B5#,
      15 => 16#B5470917#); 

   -------------------------------------------
   --  Initial values ( IV32for BLAKE-32)  --
   -------------------------------------------

   IV32 : DXPL_Types_32.Word_Array_8 :=
     (0 => 16#6A09E667#,
      1 => 16#BB67AE85#,
      2 => 16#3C6EF372#,
      3 => 16#A54FF53A#,
      4 => 16#510E527F#,
      5 => 16#9B05688C#,
      6 => 16#1F83D9AB#,
      7 => 16#5BE0CD19#);

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

   M : DXPL_Types_32.Word_Array_16;

   procedure G32 (A, B, C, D, I : in Integer; Message : in out Word_Array_16) is
   begin
     --# BEGIN
     Message(A) := (Message(A) + Message(B)) + 
                   (M(SIGMA(DXPL_Types_32.Rounds - 1, 2*I)) xor C32 (SIGMA (DXPL_Types_32.Rounds - 1, 2*I+1)));
     Message(D) := DXPL_Types_32.Rotate_Right (Message(D) xor Message(A), 16);
     Message(C) := Message(C) + Message(D);
     Message(B) := DXPL_Types_32.Rotate_Right (Message(B) xor Message(C), 12);
     Message(A) := (Message(A) + Message(B)) +
                   (M(SIGMA(DXPL_Types_32.Rounds - 1, 2*I+1)) xor C32 (SIGMA (DXPL_Types_32.Rounds - 1, 2*I)));
     Message(D) := DXPL_Types_32.Rotate_Right (Message(D) xor Message(A),  8);
     Message(C) := (Message(C) + Message(D));
     Message(B) := DXPL_Types_32.Rotate_Right (Message(B) xor Message(C),  7);
     --# END
   end G32;

   ----------------------------------
   --  Initialization of BLAKE-32  --
   ----------------------------------

   procedure DXPL_Initialize (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
     --  store the original message
     M(0 .. 15) := Message (0 .. 15);

     Message( 0) := IV32(0);
     Message( 1) := IV32(1);
     Message( 2) := IV32(2);
     Message( 3) := IV32(3);
     Message( 4) := IV32(4);
     Message( 5) := IV32(5);
     Message( 6) := IV32(6);
     Message( 7) := IV32(7);
     -- removed the salt
     --  Message( 8) := salt32 (0) xor c32(0);
     --  Message( 9) := salt32 (1) xor c32(1);
     --  Message(10) := salt32 (2) xor c32(2);
     --  Message(11) := salt32 (3) xor c32(3);
     Message( 8) := c32(0);
     Message( 9) := c32(1);
     Message(10) := c32(2);
     Message(11) := c32(3);
     -- if special case t=0 for the last block
     Message(12) := c32(4);
     Message(13) := c32(5);
     Message(14) := c32(6);
     Message(15) := c32(7);
     -- else
     --  Message(12) := t32(0) xor c32(4);
     --  Message(13) := t32(0) xor c32(5);
     --  Message(14) := t32(1) xor c32(6);
     --  Message(15) := t32(1) xor c32(7);
   end DXPL_Initialize;

   -------------------------------------
   --  Process one round of BLAKE-32  --
   -------------------------------------

   procedure DXPL_Process (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
     --  column step
     G32 (0, 4,  8, 12, 0, Message);
     G32 (1, 5,  9, 13, 1, Message);
     G32 (2, 6, 10, 14, 2, Message);
     G32 (3, 7, 11, 15, 3, Message);    

     --  diagonal step
     G32 (0, 5, 10, 15, 4, Message);
     G32 (1, 6, 11, 12, 5, Message);
     G32 (2, 7,  8, 13, 6, Message);
     G32 (3, 4,  9, 14, 7, Message);
   end DXPL_Process;
   
   --------------------
   --  Finalization  --
   --------------------

   procedure DXPL_Finalize (Message : in out DXPL_Types_32.Word_Array_16) is
   begin
     --  This code is not needed for this analyzation, because its
     --  overrides the initial values for a multi-block message
     --  IV32(0) := IV32(0) xor Message( 0) xor Message( 8); -- omitted xor salt32(0);
     --  IV32(1) := IV32(1) xor Message( 1) xor Message( 9); -- omitted xor salt32(1);
     --  IV32(2) := IV32(2) xor Message( 2) xor Message(10); -- omitted xor salt32(2);
     --  IV32(3) := IV32(3) xor Message( 3) xor Message(11); -- omitted xor salt32(3);
     --  IV32(4) := IV32(4) xor Message( 4) xor Message(12); -- omitted xor salt32(0);
     --  IV32(5) := IV32(5) xor Message( 5) xor Message(13); -- omitted xor salt32(1);
     --  IV32(6) := IV32(6) xor Message( 6) xor Message(14); -- omitted xor salt32(2);
     --  IV32(7) := IV32(7) xor Message( 7) xor Message(15); -- omitted xor salt32(3);
     null;
   end DXPL_Finalize;

-------------
--  SETUP  --
-------------

begin
   DXPL_Types_32.Configuration
     (DXPL_ALGORITHM   => "BLAKE-32",
      DXPL_ROUNDS      => 10,
      DXPL_TERMINATION => 256);

   --  Input Message ''
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#00800000#,
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
                       13 => 16#00000001#,
                       14 => 16#00000000#,
                       15 => 16#00000008#),
      DXPL_DIGEST  => (0  => 16#2F3908C1#,
                       1  => 16#981E99C0#,
                       2  => 16#5535E300#,
                       3  => 16#F6129132#,
                       4  => 16#FEDDE0E5#,
                       5  => 16#5614819F#,
                       6  => 16#6EBCE407#,
                       7  => 16#F00CE839#,
                       8  => 16#D8DE6580#,
                       9  => 16#A8C29C80#,
                       10 => 16#FD7D9B46#,
                       11 => 16#CC93BA39#,
                       12 => 16#E4E33F17#,
                       13 => 16#572F97E4#,
                       14 => 16#89D54063#,
                       15 => 16#B894AE8B#));

   --  Input Message: "abc"
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#64636261#,
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
      DXPL_DIGEST  => (0  => 16#718cf24e#,
                       1  => 16#7a7d593f#,
                       2  => 16#a88f9c24#,
                       3  => 16#7f7311de#,
                       4  => 16#f5502ffa#,
                       5  => 16#c28a5324#,
                       6  => 16#50c801bc#,
                       7  => 16#81a885ff#,
                       8  => 16#6507bf2a#,
                       9  => 16#ae8fd04d#,
                       10 => 16#a8e70a9b#,
                       11 => 16#5d2fc5d6#,
                       12 => 16#4b2f6a63#,
                       13 => 16#72778e0f#,
                       14 => 16#93ec20cf#,
                       15 => 16#152ed8cc#));
end BLAKE_32;

