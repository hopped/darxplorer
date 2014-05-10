with DXPL.IO.NoDebug;
with DXPL.IO.Formatter.Default;
with DXPL.IO.Logger;
with DXPL.Round_Function;
with DXPL.Types; 			use DXPL.Types;
with DXPL.Support.State;	use DXPL.Support.State;

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: Unknown
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  An abstract package defines the interface for each
--!  of the planned techqniue for analysing hash functions.
----------------------------------------------------------------
generic
	with package Round_Characteristic is new DXPL.Round_Function (<>);
	--  Enables access to the concrete statements of the hash function.
	Toolbox_Name : String;
	--  Name of the toolbox like 'Lazy Laura'
	Toolbox_Threshold : Conditioned_Float;
	--  A user-defined threshold limiting the depth of the analyzation.
	--  It can be set to 2^(-256) or something else.
	pragma Warnings (Off, Toolbox_Threshold);

	Logging_Formatter : DXPL.IO.Formatter.Object'Class;
	--  A suitable formatter to modify print outs for particular
	--  types like Conditioned_Float or Word
package DXPL.Toolbox is
	--  This package provides general functions, procedures and types
	--  to be used by its child packages. This package is abstract and
	--  and cannot be instantiated. The toolbox is used to analyze
	--  a given hash function (required declaration was done
	--  in the package Round_Characteristic, which is given here
	--  as generic parameter).
	--
	--  It is possible to analyse a hash function by means
	--  of a concrete, user-defined 'Round_State' or by
	--  means of a batch technique, which tests all possible input
	--  differences for a given hamming weight. The type
	--  for the hamming weight is defined in DXPL.Types.

	package Logger is new DXPL.IO.Logger (Logging_Formatter);
	--  Enables logging of the results produced by the analyzation.
	--  The logger can be enabled by invoking the darxploer toolbox
	--  in the command line with the parameter '-l'. For a complete
	--  list of possible parameters see DXPL.IO.Logger.

	type Object is abstract tagged limited private;

	Unsupported_Hamming_Weight : exception;
	--  This exception is raied, if a valid hamming weight
	--  might not be supported by the toolbox.

	------------------------------
	-- Functions and Procedures --
	------------------------------
	use Round_Characteristic;

	procedure Analyse (Technique : in Object; Current_State : in out Round_State) is abstract;
	--  Handles the concrete analyzation of a given hash function. As input serves
	--  a round state, which encapsulates the difference, initial round, offset and
	--  many other parameters needed to process one round of a hash function. This
	--  procedure has to be implemented for each of the supported techniques like
	--  Greedy Grete, Pedantic Petra, etc. to ensure the desired behaviour.

	procedure Batch_Analyse (Technique : in Object; Weight : in Hamming_Weight);
	--  For a given hamming weight, all possible differentials will be created.
	--  For each differential, the procedure 'Analyse' is called.

	function Image (Technique : in Object) return String;
	--  Returns a string representation of this package. Actually,
	--  it is just the name of the toolbox like Lazy Laura or
	--  Greedy Grete. Often used for debugging or logging aspects.

------------------
-- Private Part --
-------------------

private
	package Debug renames DXPL.IO.NoDebug;
	--  The debug package can be accessed by old child packages.
	pragma Warnings (Off, Debug);

	State_Alias : aliased Round_State;
	--  An alias to the 'Round_State'. It is used to handle
	--  the change in the round state from statement to statement.

	type Object is abstract tagged limited null record;
	--  This toolbox does not need a concrete record. It
	--  is given for inheritance purposes, only.

	type Message_Block is array (1 .. State_Alias.Message'Size) of Boolean;
	--  This type is able to hold a complete message for
	--  a hash function at once. Instead of declaring an array
	--  with eight 32 bit blocks, we can declare one message block
	--  of size 256 bit. This makes the creation of differentials
	--  of a particular hamming weight much easier.

end DXPL.Toolbox;
