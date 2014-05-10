--!  sha512.adb
--!
--!  Copyright 2009 Bauhaus-University Weimar, Germany
--!                 Chair of Media Security / Stefan Lucks
--!
--!  This program is free software; you can redistribute it and/or modify
--!  it under the terms of the GNU General Public License as published by
--!  the Free Software Foundation; either version 2 of the License, or
--!  (at your option) any later version.
--!
--!  This program is distributed in the hope that it will be usefu#,
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
--!  Created       :: 09/12/2009
--!  Last-Modified :: 09/12/2009
--!
--!  Description of 'SHA-512' based on the reference implementation.
------------------------------------------------------------------------
procedure SHA512 is

   ------------------------------------------------
   --  Initial double chaining pipe of SHA-512  --
   ------------------------------------------------

   Default_IV : constant DXPL_Types_64.Word_Array_8 :=
     (0 => 16#6a09e667f3bcc908#,  -- a
      1 => 16#bb67ae8584caa73b#,  -- b
      2 => 16#3c6ef372fe94f82b#,  -- c
      3 => 16#a54ff53a5f1d36f1#,  -- d
      4 => 16#510e527fade682d1#,  -- e
      5 => 16#9b05688c2b3e6c1f#,  -- f
      6 => 16#1f83d9abfb41bd6b#,  -- g
      7 => 16#5be0cd19137e2179#); -- h

   ------------------------------------
   --  Constants used in each round  --
   ------------------------------------
   
   Round_Constants : DXPL_Types_64.Word_Array_80 :=
     (0  => 16#428a2f98d728ae22#,
      1  => 16#7137449123ef65cd#,
      2  => 16#b5c0fbcfec4d3b2f#,
      3  => 16#e9b5dba58189dbbc#,
      4  => 16#3956c25bf348b538#,
      5  => 16#59f111f1b605d019#,
      6  => 16#923f82a4af194f9b#,
      7  => 16#ab1c5ed5da6d8118#,
      8  => 16#d807aa98a3030242#,
      9  => 16#12835b0145706fbe#,
      10 => 16#243185be4ee4b28c#,
      11 => 16#550c7dc3d5ffb4e2#,
      12 => 16#72be5d74f27b896f#,
      13 => 16#80deb1fe3b1696b1#,
      14 => 16#9bdc06a725c71235#,
      15 => 16#c19bf174cf692694#,
      16 => 16#e49b69c19ef14ad2#,
      17 => 16#efbe4786384f25e3#,
      18 => 16#0fc19dc68b8cd5b5#,
      19 => 16#240ca1cc77ac9c65#,
      20 => 16#2de92c6f592b0275#,
      21 => 16#4a7484aa6ea6e483#,
      22 => 16#5cb0a9dcbd41fbd4#,
      23 => 16#76f988da831153b5#,
      24 => 16#983e5152ee66dfab#,
      25 => 16#a831c66d2db43210#,
      26 => 16#b00327c898fb213f#,
      27 => 16#bf597fc7beef0ee4#,
      28 => 16#c6e00bf33da88fc2#,
      29 => 16#d5a79147930aa725#,
      30 => 16#06ca6351e003826f#,
      31 => 16#142929670a0e6e70#,
      32 => 16#27b70a8546d22ffc#,
      33 => 16#2e1b21385c26c926#,
      34 => 16#4d2c6dfc5ac42aed#,
      35 => 16#53380d139d95b3df#,
      36 => 16#650a73548baf63de#,
      37 => 16#766a0abb3c77b2a8#,
      38 => 16#81c2c92e47edaee6#,
      39 => 16#92722c851482353b#,
      40 => 16#a2bfe8a14cf10364#,
      41 => 16#a81a664bbc423001#,
      42 => 16#c24b8b70d0f89791#,
      43 => 16#c76c51a30654be30#,
      44 => 16#d192e819d6ef5218#,
      45 => 16#d69906245565a910#,
      46 => 16#f40e35855771202a#,
      47 => 16#106aa07032bbd1b8#,
      48 => 16#19a4c116b8d2d0c8#,
      49 => 16#1e376c085141ab53#,
      50 => 16#2748774cdf8eeb99#,
      51 => 16#34b0bcb5e19b48a8#,
      52 => 16#391c0cb3c5c95a63#,
      53 => 16#4ed8aa4ae3418acb#,
      54 => 16#5b9cca4f7763e373#,
      55 => 16#682e6ff3d6b2b8a3#,
      56 => 16#748f82ee5defb2fc#,
      57 => 16#78a5636f43172f60#,
      58 => 16#84c87814a1f0ab72#,
      59 => 16#8cc702081a6439ec#,
      60 => 16#90befffa23631e28#,
      61 => 16#a4506cebde82bde9#,
      62 => 16#bef9a3f7b2c67915#,
      63 => 16#c67178f2e372532b#,
      64 => 16#ca273eceea26619c#,
      65 => 16#d186b8c721c0c207#,
      66 => 16#eada7dd6cde0eb1e#,
      67 => 16#f57d4f7fee6ed178#,
      68 => 16#06f067aa72176fba#,
      69 => 16#0a637dc5a2c898a6#,
      70 => 16#113f9804bef90dae#,
      71 => 16#1b710b35131c471b#,
      72 => 16#28db77f523047d84#,
      73 => 16#32caab7b40c72493#,
      74 => 16#3c9ebe0a15c9bebc#,
      75 => 16#431d67c49c100d4c#,
      76 => 16#4cc5d4becb3e42b6#,
      77 => 16#597f299cfc657e2a#,
      78 => 16#5fcb6fab3ad6faec#,
      79 => 16#6c44198c4a475817#);

   function Ch (X, Y, Z : in DXPL_Types_64.Word) return DXPL_Types_64.Word is
   begin
      return (X and Y) xor ((not X) and Z);
   end Ch;

   function Maj (X, Y, Z : in DXPL_Types_64.Word) return DXPL_Types_64.Word is
   begin
      return (X and Y) xor (X and Z) xor (Y and Z);
   end Maj;

   function Sum0 (X : in DXPL_Types_64.Word) return DXPL_Types_64.Word is
   begin
      return DXPL_Types_64.Rotate_Right (X, 28) xor
             DXPL_Types_64.Rotate_Right (X, 34) xor
             DXPL_Types_64.Rotate_Right (X, 39);
   end Sum0;

   function Sum1 (X : in DXPL_Types_64.Word) return DXPL_Types_64.Word is
   begin
      return DXPL_Types_64.Rotate_Right (X, 14) xor
             DXPL_Types_64.Rotate_Right (X, 18) xor
             DXPL_Types_64.Rotate_Right (X, 41);
   end Sum1;

   function Sigma0 (X : in DXPL_Types_64.Word) return DXPL_Types_64.Word is
   begin
      return DXPL_Types_64.Rotate_Right (X, 1) xor
             DXPL_Types_64.Rotate_Right (X, 8) xor
             DXPL_Types_64.Shift_Right  (X, 7);
   end Sigma0;

   function Sigma1 (X : in DXPL_Types_64.Word) return DXPL_Types_64.Word is
   begin
      return DXPL_Types_64.Rotate_Right (X, 19) xor
             DXPL_Types_64.Rotate_Right (X, 61) xor
             DXPL_Types_64.Shift_Right  (X,  6);
   end Sigma1;

   ------------------
   --  Initialize  --
   ------------------

   procedure DXPL_Initialize
     (Message : in     DXPL_Types_64.Word_Array_16;
      X       : in out DXPL_Types_64.Word_Array_80;
      IV      : in out DXPL_Types_64.Word_Array_8) is
   begin
      for I in 0 .. 15 loop
         X (I) := Message (I);
      end loop;
      
      --  expand 16 word block into 80 word blocks.
      for I in 16 .. 79 loop
         X (I) := Sigma1 (X (I - 2)) + X (I - 7) + Sigma0 (X (I - 15)) + X (I - 16);
      end loop;

      IV := Default_IV;
   end DXPL_Initialize;

   ------------------------------------
   --  Process one round of SHA-512  --
   ------------------------------------

   procedure DXPL_Process
     (X  : in out DXPL_Types_64.Word_Array_80;
      IV : in out DXPL_Types_64.Word_Array_8) is
   begin
      --# BEGIN
      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds - 1
      IV (7) := IV (7) +
                Sum1 (IV (4)) +
                Ch (IV (4), IV (5), IV (6)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1)) +
                X (8 * (DXPL_Types_64.Rounds - 1));
      IV (3) := IV (3) + IV (7);
      IV (7) := IV (7) + Sum0 (IV (0)) + Maj (IV (0), IV (1), IV (2));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds
      IV (6) := IV (6) +
                Sum1 (IV (3)) +
                Ch (IV (3), IV (4), IV (5)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 1) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 1);
      IV (2) := IV (2) + IV (6);
      IV (6) := IV (6) + Sum0 (IV (7)) + Maj (IV (7), IV (0), IV (1));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds + 1
      IV (5) := IV (5) +
                Sum1 (IV (2)) +
                Ch (IV (2), IV (3), IV (4)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 2) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 2);
      IV (1) := IV (1) + IV (5);
      IV (5) := IV (5) + Sum0 (IV (6)) + Maj (IV (6), IV (7), IV (0));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds + 2
      IV (4) := IV (4) +
                Sum1 (IV (1)) +
                Ch (IV (1), IV (2), IV (3)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 3) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 3);
      IV (0) := IV (0) + IV (4);
      IV (4) := IV (4) + Sum0 (IV (5)) + Maj (IV (5), IV (6), IV (7));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds + 3
      IV (3) := IV (3) +
                Sum1 (IV (0)) +
                Ch (IV (0), IV (1), IV (2)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 4) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 4);
      IV (7) := IV (7) + IV (3);
      IV (3) := IV (3) + Sum0 (IV (4)) + Maj (IV (4), IV (5), IV (6));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds + 4
      IV (2) := IV (2) +
                Sum1 (IV (7)) +
                Ch (IV (7), IV (0), IV (1)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 5) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 5);
      IV (6) := IV (6) + IV (2);
      IV (2) := IV (2) + Sum0 (IV (3)) + Maj (IV (3), IV (4), IV (5));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds + 5
      IV (1) := IV (1) +
                Sum1 (IV (6)) +
                Ch (IV (6), IV (7), IV (0)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 6) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 6);
      IV (5) := IV (5) + IV (1);
      IV (1) := IV (1) + Sum0 (IV (2)) + Maj (IV (2), IV (3), IV (4));

      -- DXPL_Types_64.Rounds = 8 * DXPL_Types_64.Rounds + 6
      IV (0) := IV (0) +
                Sum1 (IV (5)) +
                Ch (IV (5), IV (6), IV (7)) +
                Round_Constants (8 * (DXPL_Types_64.Rounds - 1) + 7) +
                X (8 * (DXPL_Types_64.Rounds - 1) + 7);
      IV (4) := IV (4) + IV (0);
      IV (0) := IV (0) + Sum0 (IV (1)) + Maj (IV (1), IV (2), IV (3));
      --# END
   end DXPL_Process;

   ----------------
   --  Finalize  --
   ----------------

   procedure DXPL_Finalize
     (Message : in out DXPL_Types_64.Word_Array_16;
      IV      : in     DXPL_Types_64.Word_Array_8) is
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
   DXPL_Types_64.Configuration
     (DXPL_ALGORITHM   => "SHA-512",
      DXPL_ROUNDS      => 10,
      DXPL_TERMINATION => 512);

   --  Input Message: ''
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#8000000000000000#,
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
                       15 => 16#0000000000000000#),
      DXPL_DIGEST  => (0  => 16#cf83e1357eefb8bd#,
                       1  => 16#f1542850d66d8007#,
                       2  => 16#d620e4050b5715dc#,
                       3  => 16#83f4a921d36ce9ce#,
                       4  => 16#47d0d13c5d85f2b0#,
                       5  => 16#ff8318d2877eec2f#,
                       6  => 16#63b931bd47417a81#,
                       7  => 16#a538327af927da3e#));

   --  Input Message: 'a'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#6180000000000000#,
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
                       15 => 16#0000000000000008#),
      DXPL_DIGEST  => (0  => 16#1f40fc92da241694#,
                       1  => 16#750979ee6cf582f2#,
                       2  => 16#d5d7d28e18335de0#,
                       3  => 16#5abc54d0560e0f53#,
                       4  => 16#02860c652bf08d56#,
                       5  => 16#0252aa5e74210546#,
                       6  => 16#f369fbbbce8c12cf#,
                       7  => 16#c7957b2652fe9a75#));

   --  Input Message: 'abc'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#6162638000000000#,
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
      DXPL_DIGEST  => (0  => 16#ddaf35a193617aba#,
                       1  => 16#cc417349ae204131#,
                       2  => 16#12e6fa4e89a97ea2#,
                       3  => 16#0a9eeee64b55d39a#,
                       4  => 16#2192992a274fc1a8#,
                       5  => 16#36ba3c23a3feebbd#,
                       6  => 16#454d4423643ce80e#,
                       7  => 16#2a9ac94fa54ca49f#));

   --  Input Message: 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn'
   --                 'hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
   DXPL_Types_64.Test_Vector
     (DXPL_MESSAGE => (0  => 16#6162636465666768#,
                       1  => 16#6263646566676869#,
                       2  => 16#636465666768696a#,
                       3  => 16#6465666768696a6b#,
                       4  => 16#65666768696a6b6c#,
                       5  => 16#666768696a6b6c6d#,
                       6  => 16#6768696a6b6c6d6e#,
                       7  => 16#68696a6b6c6d6e6f#,
                       8  => 16#696a6b6c6d6e6f70#,
                       9  => 16#6a6b6c6d6e6f7071#,
                       10 => 16#6b6c6d6e6f707172#,
                       11 => 16#6c6d6e6f70717273#,
                       12 => 16#6d6e6f7071727374#,
                       13 => 16#6e6f707172737475#,
                       14 => 16#8000000000000000#,
                       15 => 16#0000000000000000#),
      DXPL_DIGEST  => (0  => 16#4319017a2b706e69#,
                       1  => 16#cd4b05938bae5e89#,
                       2  => 16#0186bf199f30aa95#,
                       3  => 16#6ef8b71d2f810585#,
                       4  => 16#d787d6764b20bda2#,
                       5  => 16#a260144709736920#,
                       6  => 16#00ec057f37d14b8e#,
                       7  => 16#06add5b50e671c72#));
end SHA512;
