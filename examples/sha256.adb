--!  sha256.adb
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
--!  Created       :: 05/12/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of 'SHA-256' based on the reference implementation.
------------------------------------------------------------------------
procedure SHA256 is

   ------------------------------------------------
   --  Initial double chaining pipe of SHAA-256  --
   ------------------------------------------------

   Default_IV : constant DXPL_Types_32.Word_Array_8 :=
     (0 => 16#6a09e667#,
      1 => 16#bb67ae85#,
      2 => 16#3c6ef372#,
      3 => 16#a54ff53a#,
      4 => 16#510e527f#,
      5 => 16#9b05688c#,
      6 => 16#1f83d9ab#,
      7 => 16#5be0cd19#);

   ------------------------------------
   --  Constants used in each round  --
   ------------------------------------

   Round_Constants : DXPL_Types_32.Word_Array_64 :=
     (0  => 16#428a2f98#,
      1  => 16#71374491#,
      2  => 16#b5c0fbcf#,
      3  => 16#e9b5dba5#,
      4  => 16#3956c25b#,
      5  => 16#59f111f1#,
      6  => 16#923f82a4#,
      7  => 16#ab1c5ed5#,
      8  => 16#d807aa98#,
      9  => 16#12835b01#,
      10 => 16#243185be#,
      11 => 16#550c7dc3#,
      12 => 16#72be5d74#,
      13 => 16#80deb1fe#,
      14 => 16#9bdc06a7#,
      15 => 16#c19bf174#,
      16 => 16#e49b69c1#,
      17 => 16#efbe4786#,
      18 => 16#0fc19dc6#,
      19 => 16#240ca1cc#,
      20 => 16#2de92c6f#,
      21 => 16#4a7484aa#,
      22 => 16#5cb0a9dc#,
      23 => 16#76f988da#,
      24 => 16#983e5152#,
      25 => 16#a831c66d#,
      26 => 16#b00327c8#,
      27 => 16#bf597fc7#,
      28 => 16#c6e00bf3#,
      29 => 16#d5a79147#,
      30 => 16#06ca6351#,
      31 => 16#14292967#,
      32 => 16#27b70a85#,
      33 => 16#2e1b2138#,
      34 => 16#4d2c6dfc#,
      35 => 16#53380d13#,
      36 => 16#650a7354#,
      37 => 16#766a0abb#,
      38 => 16#81c2c92e#,
      39 => 16#92722c85#,
      40 => 16#a2bfe8a1#,
      41 => 16#a81a664b#,
      42 => 16#c24b8b70#,
      43 => 16#c76c51a3#,
      44 => 16#d192e819#,
      45 => 16#d6990624#,
      46 => 16#f40e3585#,
      47 => 16#106aa070#,
      48 => 16#19a4c116#,
      49 => 16#1e376c08#,
      50 => 16#2748774c#,
      51 => 16#34b0bcb5#,
      52 => 16#391c0cb3#,
      53 => 16#4ed8aa4a#,
      54 => 16#5b9cca4f#,
      55 => 16#682e6ff3#,
      56 => 16#748f82ee#,
      57 => 16#78a5636f#,
      58 => 16#84c87814#,
      59 => 16#8cc70208#,
      60 => 16#90befffa#,
      61 => 16#a4506ceb#,
      62 => 16#bef9a3f7#,
      63 => 16#c67178f2#);

   function Ch (X, Y, Z : in DXPL_Types_32.Word) return DXPL_Types_32.Word is
   begin
      return (X and Y) xor ((not X) and Z);
   end Ch;

   function Maj (X, Y, Z : in DXPL_Types_32.Word) return DXPL_Types_32.Word is
   begin
      return (X and Y) xor (X and Z) xor (Y and Z);
   end Maj;

   function Sum0 (X : in DXPL_Types_32.Word) return DXPL_Types_32.Word is
   begin
      return DXPL_Types_32.Rotate_Right (X, 2) xor
             DXPL_Types_32.Rotate_Right (X, 13) xor
             DXPL_Types_32.Rotate_Right (X, 22);
   end Sum0;

   function Sum1 (X : in DXPL_Types_32.Word) return DXPL_Types_32.Word is
   begin
      return DXPL_Types_32.Rotate_Right (X, 6) xor
             DXPL_Types_32.Rotate_Right (X, 11) xor
             DXPL_Types_32.Rotate_Right (X, 25);
   end Sum1;

   function Theta0 (X : in DXPL_Types_32.Word) return DXPL_Types_32.Word is
   begin
      return DXPL_Types_32.Rotate_Right (X, 7) xor
             DXPL_Types_32.Rotate_Right (X, 18) xor
             DXPL_Types_32.Shift_Right  (X, 3);
   end Theta0;

   function Theta1 (X : in DXPL_Types_32.Word) return DXPL_Types_32.Word is
   begin
      return DXPL_Types_32.Rotate_Right (X, 17) xor
             DXPL_Types_32.Rotate_Right (X, 19) xor
             DXPL_Types_32.Shift_Right  (X, 10);
   end Theta1;

   ------------------
   --  Initialize  --
   ------------------

   procedure DXPL_Initialize
     (Message : in     DXPL_Types_32.Word_Array_16;
      X       : in out DXPL_Types_32.Word_Array_64;
      IV      : in out DXPL_Types_32.Word_Array_8) is
   begin
      for I in 0 .. 15 loop
         X (I) := Message (I);
      end loop;

      --  expand 16 word block into 64 word blocks.
      for I in 16 .. 63 loop
         X (I) := Theta1 (X (I - 2)) + X (I - 7) + Theta0 (X (I - 15)) + X (I - 16);
      end loop;

      IV := Default_IV;
   end DXPL_Initialize;

   ------------------------------------
   --  Process one round of SHA-256  --
   ------------------------------------

   procedure DXPL_Process
     (X  : in out DXPL_Types_32.Word_Array_64;
      IV : in out DXPL_Types_32.Word_Array_8) is
   begin
      --# BEGIN
      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds - 1
      IV (7) := IV (7) +
                Sum1 (IV (4)) +
                Ch (IV (4), IV (5), IV (6)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1)) +
                X (8 * (DXPL_Types_32.Rounds - 1));
      IV (3) := IV (3) + IV (7);
      IV (7) := IV (7) + Sum0 (IV (0)) + Maj (IV (0), IV (1), IV (2));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds
      IV (6) := IV (6) +
                Sum1 (IV (3)) +
                Ch (IV (3), IV (4), IV (5)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 1) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 1);
      IV (2) := IV (2) + IV (6);
      IV (6) := IV (6) + Sum0 (IV (7)) + Maj (IV (7), IV (0), IV (1));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds + 1
      IV (5) := IV (5) +
                Sum1 (IV (2)) +
                Ch (IV (2), IV (3), IV (4)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 2) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 2);
      IV (1) := IV (1) + IV (5);
      IV (5) := IV (5) + Sum0 (IV (6)) + Maj (IV (6), IV (7), IV (0));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds + 2
      IV (4) := IV (4) +
                Sum1 (IV (1)) +
                Ch (IV (1), IV (2), IV (3)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 3) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 3);
      IV (0) := IV (0) + IV (4);
      IV (4) := IV (4) + Sum0 (IV (5)) + Maj (IV (5), IV (6), IV (7));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds + 3
      IV (3) := IV (3) +
                Sum1 (IV (0)) +
                Ch (IV (0), IV (1), IV (2)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 4) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 4);
      IV (7) := IV (7) + IV (3);
      IV (3) := IV (3) + Sum0 (IV (4)) + Maj (IV (4), IV (5), IV (6));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds + 4
      IV (2) := IV (2) +
                Sum1 (IV (7)) +
                Ch (IV (7), IV (0), IV (1)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 5) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 5);
      IV (6) := IV (6) + IV (2);
      IV (2) := IV (2) + Sum0 (IV (3)) + Maj (IV (3), IV (4), IV (5));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds + 5
      IV (1) := IV (1) +
                Sum1 (IV (6)) +
                Ch (IV (6), IV (7), IV (0)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 6) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 6);
      IV (5) := IV (5) + IV (1);
      IV (1) := IV (1) + Sum0 (IV (2)) + Maj (IV (2), IV (3), IV (4));

      -- DXPL_Types_32.Rounds = 8 * DXPL_Types_32.Rounds + 6
      IV (0) := IV (0) +
                Sum1 (IV (5)) +
                Ch (IV (5), IV (6), IV (7)) +
                Round_Constants (8 * (DXPL_Types_32.Rounds - 1) + 7) +
                X (8 * (DXPL_Types_32.Rounds - 1) + 7);
      IV (4) := IV (4) + IV (0);
      IV (0) := IV (0) + Sum0 (IV (1)) + Maj (IV (1), IV (2), IV (3));
      --# END
   end DXPL_Process;

   ----------------
   --  Finalize  --
   ----------------

   procedure DXPL_Finalize
     (Message : in out DXPL_Types_32.Word_Array_16;
      IV      : in     DXPL_Types_32.Word_Array_8) is
   begin
      Message (0) := IV (0) + Default_IV (0);
      Message (1) := IV (1) + Default_IV (1);
      Message (2) := IV (2) + Default_IV (2);
      Message (3) := IV (3) + Default_IV (3);
      Message (4) := IV (4) + Default_IV (4);
      Message (5) := IV (5) + Default_IV (5);
      Message (6) := IV (6) + Default_IV (6);
      Message (7) := IV (7) + Default_IV (7);
   end DXPL_Finalize;

------------
--  SETUP --
------------

begin
   DXPL_Types_32.Configuration
     (DXPL_ALGORITHM   => "SHA-256",
      DXPL_ROUNDS      => 8,
      DXPL_TERMINATION => 256);

   --  Input Message:
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#00000000#,
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
      DXPL_DIGEST  => (0  => 16#da5698be#,
                       1  => 16#17b9b469#,
                       2  => 16#62335799#,
                       3  => 16#779fbeca#,
                       4  => 16#8ce5d491#,
                       5  => 16#c0d26243#,
                       6  => 16#bafef9ea#,
                       7  => 16#1837a9d8#));

   -- Input Message: ''
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#80000000#,
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
      DXPL_DIGEST  => (0  => 16#e3b0c442#,
                       1  => 16#98fc1c14#,
                       2  => 16#9afbf4c8#,
                       3  => 16#996fb924#,
                       4  => 16#27ae41e4#,
                       5  => 16#649b934c#,
                       6  => 16#a495991b#,
                       7  => 16#7852b855#));

   -- Input Message: 'a'
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#61800000#,
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
                       15 => 16#00000008#),
      DXPL_DIGEST  => (0  => 16#ca978112#,
                       1  => 16#ca1bbdca#,
                       2  => 16#fac231b3#,
                       3  => 16#9a23dc4d#,
                       4  => 16#a786eff8#,
                       5  => 16#147c4e72#,
                       6  => 16#b9807785#,
                       7  => 16#afee48bb#));

   -- Input Message: 'abc'
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#61626380#,
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
                       15 => 16#00000018#),
      DXPL_DIGEST  => (0  => 16#ba7816bf#,
                       1  => 16#8f01cfea#,
                       2  => 16#414140de#,
                       3  => 16#5dae2223#,
                       4  => 16#b00361a3#,
                       5  => 16#96177a9c#,
                       6  => 16#b410ff61#,
                       7  => 16#f20015ad#));

   --  Input Message: 'abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq'
   DXPL_Types_32.Test_Vector
     (DXPL_MESSAGE => (0  => 16#61626364#,
                       1  => 16#62636465#,
                       2  => 16#63646566#,
                       3  => 16#64656667#,
                       4  => 16#65666768#,
                       5  => 16#66676869#,
                       6  => 16#6768696a#,
                       7  => 16#68696a6b#,
                       8  => 16#696a6b6c#,
                       9  => 16#6a6b6c6d#,
                       10 => 16#6b6c6d6e#,
                       11 => 16#6c6d6e6f#,
                       12 => 16#6d6e6f70#,
                       13 => 16#6e6f7071#,
                       14 => 16#80000000#,
                       15 => 16#00000000#),
      DXPL_DIGEST  => (0  => 16#85e655d6#,
                       1  => 16#417a1795#,
                       2  => 16#3363376a#,
                       3  => 16#624cde5c#,
                       4  => 16#76e09589#,
                       5  => 16#cac5f811#,
                       6  => 16#cc4b32c1#,
                       7  => 16#f20e533a#));
end SHA256;
