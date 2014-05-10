----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of the validation technqiue for
--!  hash functions called 'Trusty Trudy'.
----------------------------------------------------------------
generic
	--  omitted
package DXPL.Toolbox.Trudy is

	type Instance is new Toolbox.Object with private;

	------------------------------
	-- Functions and Procedures --
	------------------------------

	overriding
	procedure Analyse (Technique : in Instance; Current_State : in out Round_State);
	--  See DXPL.Toolbox for general information. Trusty Trudy implements
	--  the 'Analyse' procedure in such a way, that it will follow straight the
	--  steps of processing a hash function by this framework: Initialize,
	--  Process all arithmetic statements, Finalize. The processing is done
	--  as described in the description of the hash function. Be more specific,
	--  this procedure does, what you would expect from the original hash function.
	--
	--  Overview of procedue in pseudo code:
	--
	--  initialize;
	--  for all rounds loop
	--    for all statements loop
	--      process;
	--    end loop;
	--  end loop;
	--  finalize;

	not overriding
	function Is_Valid (Technique : in Instance) return Boolean;
	--  Validates the transformation of the descripiton of the
	--  given hash function. A user has to declare some test vectors,
	--  for a proper validation. If all test vectors produce
	--  the desired output for the given input, this function
	--  returns 'True'.
	--
	--  This function is unique to the toolbox of Trusty Trudy
	--  and violates in some kinds the generic constructions of
	--  the toolbox. Actually, all techniques (LL, GG, PP) should
	--  be allowed to test the transformation of the hash function
	--  before doing the analyzation stuff.
	--
	--  TODO: This function should tell the user, which
	--  test vector does not produce the expected result, if
	--  more than one are declared.

------------------
-- Private Part --
-------------------

private
	type Instance is new Toolbox.Object with null record;

end DXPL.Toolbox.Trudy;
