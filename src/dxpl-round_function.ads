with DXPL.Types;			use DXPL.Types;
with DXPL.Support.State;	use DXPL.Support.State;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/25/2009

--!  Purpose:
--!  Representation of a round often used by hash functions. It
--!  provides functions to access the input message, the resul-
--!  ting output digest and the given key. It also provides
--!  methods Initalize and Finalize, which will be executed
--!  at the beginning respectively in the end, after all rounds
--!  are processed.
--!
--!  A procedure called 'Analyse' can be passed as a generic
--!  parameter to alternate the behaviour of the analyzation of
--!  the underlying round function. See the package DXPL.Technique
--!  for possible implementations of 'Analyse'.
----------------------------------------------------------------
generic	
	with procedure Analyse (X, Y			  : in     Word;
							Z				  : in out Word;
							Output_Candidates : in out Word_Vector.Vector;
							Probability       : in out Conditioned_Float);
	Number_of_Modules : Integer;
package DXPL.Round_Function is

	type Process_Module is access function (State : access Round_State) return access Word;
	Module_Sequence : array (1 .. Number_of_Modules) of Process_Module;

	subtype Module_Offset is Integer range Module_Sequence'First .. Module_Sequence'Last;

	------------------------------
	-- Functions and Procedures --
	------------------------------

	function Get_Message (Index : in Positive) return Word_Array;
	--  Returns the initial value given by the description of the hash function.
	--  It is possible to declare more than one input message. Access is
	--  supported by means of an index. An initial value is often used
	--  in combination with a digest to form a test vector. Test vectors
	--  are used to validate the hash function.

	function Get_Digest  (Index : in Positive) return Word_Array;
	--  Returns the digest of the hashing.

	function Get_Key     (Index : in Positive) return Word_Array;
	--  Returns a given key (optional).

	function Number_of_Tests return Positive;
	--  Returns the number of test vectors available.

	procedure Initialize (State : access Round_State);
	--  This procedure will be executed before the first round is processed.
	--  Used it to define general statements that should be processed
	--  in the first instance like assignment the correct initial key.

	procedure Finalize   (State : access Round_State);
	--  This procedure will be called after all rounds are processed. It
	--  can be installed to reassign a temporary value to the output value.

------------------
-- Private Part --
-------------------

private
	type Validation is 
	--  Represents a test vector
		record
			Message : access Word_Array;
			Key     : access Word_Array;
			Digest  : access Word_Array;
		end record;
	Test_Vector : Validation;
	
	type Validation_Array is array (Positive range <>) of Validation;
	type Batch_Validation is
	--  Represents a battery of test vectors for batch validation
		record
			Batch : access Validation_Array;
		end record;
	Test_Vectors : Batch_Validation;
end DXPL.Round_Function;