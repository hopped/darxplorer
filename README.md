# DARXPLORER 2.0 - a Toolbox for Cryptanalysts and Cipher Designers

This is the source distribution of DARXplorer 2.0.

> __NOTE:__ This was a research project during my study course back in 2009. I was able
to get the source code running successfully for __Threefish 256__ under Mac OS X 10.9 using the 2009 version of
the [GNU Ada compiler](http://sourceforge.net/projects/gnuada/files/GNAT_GPL%20Mac%20OS%20X/2009-snow-leopard/). You might
want to install an equivalent version of that compiler on your system, because
the code is based on Ada 2005. Special thanks to my fellow students at that time
and to [Stefan Lucks](http://www.uni-weimar.de/de/medien/professuren/mediensicherheit/people/stefan-lucks/).


## Introduction

DARXplorer supports the differential cryptanalysis of primitives which
use a mixture of addition, rotation and XOR. DARXplorer attempts to find
good XOR differences. The two main use cases for DARXplorer are

1. analysing existing primitives to find attacks and
2. aiding the designers of new primitives to defend against attacks.

The first version of DARXplorer aimed mainly at supporting the
development of the Threefish block cipher used in the hash function
Skein. Now, the second version of DARXplorer is able to analyse
arbitrary cryptographic primitives based on the previous stated mixture
of operations.

Cryptanalysts are often confronted with new primitives demanding new
tools for analysis. DARXplorer offers a convenient way to describe
such a primitive directly in Ada at minimal costs. The whole analysing
framework including extensive logging mechanisms is already at ones
feet.

As it turned out, the Achilles heel of these primitives was some
vulnerability to differential cryptanalysis, mostly based on XOR
differences. When analysing a given primitive of this type, searching
for good XOR differences appears to be the way to go.

The toolbox will implement four different techniques to find "good"
characteristics. Usually, the differential behaviour of a low
Hamming-weight difference is better predictable (i.e., with a higher
probability) than the behaviour of a "random" difference. We just start
with such a low Hamming-weight difference (1, 2, 3 or 4) and try all
possible input differences with the chosen Hamming-weight.


### Lazy Laura

Laura just "guesses" that the the addition behaves like the XOR,
difference-wise. I.e., there are no carry bits generated. Starting with
low Hamming-weight differences, it is very easy for her to compute the
output difference (running the primitive in forward direction). The
benefit of Laura's approach is that it is very efficient and can be
implemented for any number of rounds (for 80 rounds, or for 800 rounds).


### Greedy Grete

Laura just assumes no carry bits are generated. This is likely to
exclude many "good" characteristics---a carry bit in one round leads
to another difference in another round and may ultimately lead to a
better (more probable) difference at the end. Grete optimises locally.
Given some round's input differences in some round, she enumerates all
the most probable output differences. All of them are considered as
the input difference for the next round. If there is more than one
optimal difference, Grete's run time will grow exponentially with the
number of rounds.


### Pedantic Petra

Petra goes a step further than Grete. Given an input difference, she
enumerates all output differences with a nonzero probability. Recall
that this is feasible as long as the Hamming-weight of the difference
is low. As the number of possible differences is growing fast, and
the Hamming-weight also grows with the number of rounds, this can be
done only for a few rounds. Petra is pedantic, but not stupid. A naive
approach would make her deal with a huge number of extremely
low-probable characteristics. But Petra sorts her intermediate results
(characteristics with less than the desired number of rounds) by
their probability. She always proceeds with the most probable
intermediate result. Specifically, when she comes to the last round
she just uses Grete's approach.


### Fuzzy Fiona

Fiona's strategy is a new crypto-analytical approach. Assuming that
the input for a S-Box, i.e. for a non-linear function, is 100101.
The S-Box describes a mapping from 6 bits to 4 bits. However,
the output can only be defined exactly for particular bits: 1??0,
whereby question marks represent bits, whose value cannot be
clearly fixed. Even so, one can specify the probability, that a bit
equals 1. Now, rewriting the output with probabilities results
in (1, 1/4, 2/3, 0). The third bit takes on value 1 with probability
2/3, and the last bit is definitely 0 and is never equals 1.
Pursuing such probabilities while computing a hash digest, let one
employ predictions about the output, if the probability is distinct
from 1/2.


## Supported Hash functions

This source distribution already has default descriptions for the following
hash functions (cf. __examples__ folder):

- Blake-32
- Blake-64
- Blue Midnight Wish 256
- Blue Midnight Wish 512
- Edon-R 256
- Edon-R 512
- Salsa-20
- SHA-256
- SHA-512
- Tiny Encryption Algorithm (TEA)
- Threefish-256
- Threefish-512
- Threefish-1024

It follows as an example the description of TEA.

with DXPL_Types_32; use DXPL_Types_32;

``` ada
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
```


## Documentation

There exists extensive documentation essential to both [developers](docs/developer/darx-developer-20090924.pdf) and [users](docs/user/darx-user-20090924.pdf)
in the __docs__ folder.


## Prerequisites

All you need is GNAT, the free-software compiler for Ada in version 4.3
or higher. More information can be found at

```
http://gnuada.sourceforge.net/
```

If you are running an UNIX based operating system like Ubuntu, you can
get the current GNAT compiler simply by typing

``` bash
sudo apt-get install gnat
```

DARXplorer does not use any GNAT-specific features. It is likely, that
DARXplorer will compile with GNAT versions prior to 4.3, but we just
tested the current distribution with Ubuntu 9.04 and GNAT 4.3 installed.


## How to get started

> Last update on 09/23/2009

DARXplorer 2.0 is a comprehensive toolbox, but don't be scared. Using
DARXplorer is very easy. First, you have to draw up a description
of a cryptographic primitive in Ada. Have a look into the "examples"
directory which contains a further README file, which will illuminate
the process of declaring primitives applicable to DARXplorer.

Subsequent to the creation of a new Ada source file, you have to type

``` bash
./build.sh $PATH_TO_YOUR_SCRIPT
```

Don't miss to replace the placeholder $PATH_TO_YOUR_SCRIPT with the
actual path to your script including the filename. If the shell script
isn't running, give it the following rights and try again

``` bash
chmod u+x build.sh
```

The framework analyses your description. If it is valid and
applicable to DARXplorer, it will be integrated into the framework
and automatically validated with the aid of test vectors. You have
to provide test vectors by yourself.

The output of the shell script might look like this

``` bash
$./darxplorer.sh examples/threefish256.adb
++ The framework will be compiled .. FINISHED
++ DARXplorer will be compiled by means of your description ..
   FINISHED

   DARXplorer has validated your description.
   The application 'darxplorer' is ready to run.
```

If something went wrong, DARXplorer will tell you. :)

The newly created DARXplorer executable allows you to analyse the
described primitive, finally. To obtain all features of DARXplorer,
just type

``` bash
./darxplorer --help
```

If you execute 'darxplorer' without any arguments, the framework
validates your description on the basis of provided test vectors, again.


## About the package

DARXplorer comes with a bunch of examples. You should proceed to read
them first before beginning write your own. The directory 'tests'
includes many files to check the integrity of DARXplorer. We used
'tg', a test driver generator for Ada programs.

Besides common Ada data structures, we used most notable graph based
structures from the [Ada Booch Library](http://sourceforge.net/projects/booch95/).

for more information. The version we used is included in this
distribution and is contained in the directory 'bc-20090226'.


## Authors

DARXplorer was written by Dennis Hoppe, Julian Seifert,
Stoyan Stoyanov and Max Grosse under the supervision of Stefan Lucks at the
Bauhaus-Universität Weimar, Germany.


## Licence
Copyright (C) 2008, 2009 by

  Bauhaus-University Weimar, Germany
  [Chair of Media Security](http://www.uni-weimar.de/de/medien/professuren/mediensicherheit/home/), Stefan Lucks
