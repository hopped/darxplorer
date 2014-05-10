with DXPL.Booch.Priority_Queue;

pragma Elaborate_All (DXPL.Booch.Priority_Queue);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/24/2009
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Package provides methods to compute the differential
--!  equation of addition and it's probability for a given
--!  differential input pair (a,b) and a defined output c.
----------------------------------------------------------------
package DXPL.Support.Differential_Addition is

	------------------------------
	-- Functions and Procedures --
	------------------------------

    procedure Differential_Probability (Delta_A     : in     Word;
										Delta_B     : in     Word;
										Delta_Out   : in     Word;
										Probability : in out Conditioned_Float);
	--  Computes the differential probability for two given input values
	--  and an associated output value. Algorithm based on the definition
	--  given in 'Efficient Algorithms for Computing Differential Properties
	--  of Addition' by Lipmaa, Helger and Moriai, Shiho.
	--# pre : Delta_A := A xor A'
	--        Delta_B := B xor B'
	--# post: probability is in range 0.0 .. 1.0
	--        if probability = 0 then Delta_Out is not possible for given input deltas

	procedure Optimal_Differential_Output (Delta_A     : in     Word;
										   Delta_B     : in     Word;
										   Delta_Out   : in out Word);
	-- Finds the optimal output difference of two given input values delta_a and delta_b.
	-- Algorithm based on the definition given in 'Efficient Algorithms for Computing
	-- Differential Properties of Addition' by Lipmaa, Helger and Moriai, Shiho.

	procedure All_Optimal_Differentials (Delta_A    : in     Word;
										 Delta_B    : in     Word;
										 Delta_Out  : in out Word_Vector.Vector);
	-- Finds ALL optimal output differences of two given input values delta_a and delta_b.
	-- Algorithm based on the definition given in 'Efficient Algorithms for Computing
	-- Differential Properties of Addition' by Lipmaa, Helger and Moriai, Shiho.
	-- and the solution proposed by our Professor Stefan Lucks

	procedure All_Non_Optimal_Differentials (Delta_A     : in     Word;
											 Delta_B     : in     Word;
											 Delta_Out   : in out Word_Vector.Vector);
	--  Finds ALL output differences of two given input values delta_a and delta_b with
	--  probability unequal zero. Algorithm based on the definition given in 'Efficient 
	--  Algorithms for Computing Differential Properties of Addition' by Lipmaa, Helger
	--  and Moriai, Shiho.

------------------
-- Private Part --
-------------------

private
	------------------------------
	-- Functions and Procedures --
	------------------------------

	function EQ (Delta_A, Delta_B, Delta_Out : in Word) return Word;
	--  EQ (a,b,c) := ((not a) xor b) and ((not a) and c)
	--  used to compute the differential probability. formula is derived from
	-- 'Efficient Algorithms for Computing Differential Properties of Addition'
 	--  by Lipmaa, Helger and Moriai, Shiho.

	function Mask return Word;
	--  Mask (n) := 2**n - 1;  equals Mask (n) := -1;
	--  used to compute the differential probability. formula is derived from
	-- 'Efficient Algorithms for Computing Differential Properties of Addition'
 	--  by Lipmaa, Helger and Moriai, Shiho.

	function Log_Base_2 return Positive;
	--  computes the logarithm to base 2 of the size n of the current modular type.
	--  function does not accept parameters to avoid misuse.
	--# pragma Inline

	function All_One_Parity (Input_Value : in Word) return Word;
	--  the all-one parity of an n-bit number x is another n-bit number y = All_One_Parity(x)
	--  s.t. y_i = 1, if the longest sequence of consecutive one-bits x_i, x_i+1, .. x_j =
	--  11..1 has odd length. algorithm based on description in 'Efficient Algorithms
	--  for Computing Differential Properties of Addition' by Lipmaa, Helger and Moriai, Shiho.

	function All_One_Parity_Reverse (Input_Value : in Word) return Word;
	--  the dual of all-one parity, obtained by a bit-reversing its arguments

	function Common_Alternation_Parity (Input_A, Input_B : in Word) return Word;
	--  the common alternation parity of two n-bit number x and y is :
	--  (1) 0 when the length of of the longest common alternating bit chain is even and non-zero
	--  (2) 1 when the length of of the longest common alternating bit chain is odd
	--  (3) either 0 or 1 when the length of of the longest common alternating bit chain is 0
	--  algorithm based on description in 'Efficient Algorithms for Computing Differential
	--  Properties of Addition' by Lipmaa, Helger and Moriai, Shiho.

	--------------------------------
    -- Declare the Priority Queue --
    --------------------------------

	type Pair is record
        Element  : Word;
        Priority : Conditioned_Float;
    end record;

	function "<" (Left, Right : Pair) return Boolean;

	package Sorted_Queue is new DXPL.Booch.Priority_Queue (Element => Pair, "<" => "<", Size => 40);
	PQueue : Sorted_Queue.QB.Queue;

end DXPL.Support.Differential_Addition;
