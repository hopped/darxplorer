with DXPL.Round_Function;
with DXPL.Toolbox;
with DXPL.Toolbox.Trudy;
with DXPL.Types;  use DXPL.Types;

pragma Elaborate_All (DXPL.Round_Function);
pragma Elaborate_All (DXPL.Toolbox.Trudy);

----------------------------------------------------------------
--!  Author        :: dennis.hoppe@uni-weimar.de
--!  Created       :: 05/30/2009
--!  Last-Modified :: 08/24/2009

--!  Purpose:
--!  Implementation of 'Trusty Trudy'
----------------------------------------------------------------
generic
	Number_of_Modules : Integer;
package DXPL.Technique.Trudy is

	procedure Solve (X, Y				: in     Word;
					 Z					: in out Word;
					 Output_Candidates : in out Word_Vector.Vector;
					 Probability       : in out Conditioned_Float);
	--  This procedure is valid as a parameter to be used
	--  with the generic round function. Please don't change
	--  the list of parameters.

	package Characteristic_Function is new DXPL.Round_Function
		 (Analyse 			=> Solve,
		  Number_of_Modules => Number_of_Modules);
	--  Declaration of the round function.

	package DARX is new Toolbox
		(Round_Characteristic => Characteristic_Function,
		 Toolbox_Name		  => "Trusty Trudy",
		 Toolbox_Threshold    => Toolbox_Threshold,
		 Logging_Formatter    => Default_Formatter);
	--  Declaration of a concrete toolbox.

	package DARXplorer is new DARX.Trudy;
	Toolbox : DARXplorer.Instance;
	--  Instance of the toolbox to be used from outside.

end DXPL.Technique.Trudy;