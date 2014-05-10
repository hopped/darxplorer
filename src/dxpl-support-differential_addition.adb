with Ada.Containers;
with Ada.Numerics.Elementary_Functions;

package body DXPL.Support.Differential_Addition is
	use Sorted_Queue;

	--------
	-- EQ --
	--------

	function EQ (Delta_A, Delta_B, Delta_Out : in Word) return Word is
	begin
		return ((not Delta_A) xor Delta_B) and ((not Delta_A) xor Delta_Out);
	end EQ;
	pragma Inline (EQ);

	----------
	-- Mask --
	----------

	function Mask return Word is
	begin
		return -1;	-- 2**Word'Size - 1;
	end Mask;
	pragma Inline (Mask);

	----------------------------
	-- Log_Base_2 --
	----------------------------

	function Log_Base_2 return Positive is
	begin
		return Positive(Ada.Numerics.Elementary_Functions.Log (Float(Word'Size), 2.0));
	end Log_Base_2;
	pragma Inline (Log_Base_2);

	----------------------------
	-- All_One_Parity --
	----------------------------

	function All_One_Parity (Input_Value : in Word) return Word is
		Log_N : Positive := Log_Base_2;
		type Auxilliary_Array is array (1 .. Log_N) of Word;
		X_Array : Auxilliary_Array := (others => 2#0#);
		Y_Array : Auxilliary_Array := (others => 2#0#);
	begin
		X_Array(1) := Input_Value and Shift_Right(Input_Value, 1);
		-- step one

		for I in 2 .. Log_N -1 loop
			X_Array(I) := X_Array(I - 1) and Shift_Right(X_Array(I - 1), 2**(I - 1));
		end loop;
		-- step two

		Y_Array(1) := Input_Value and not X_Array(1);
		-- step three

		for I in 2 .. Log_N loop
			Y_Array(I) := Y_Array(I - 1) or ((Y_Array(I - 1) / 2**(2**(I - 1))) and X_Array(I - 1));
			-- (todo) shift_right does not work here...
		end loop;
		-- step four

		return Y_Array(Log_N);
	end All_One_Parity;

	----------------------------
	-- All_One_Parity_Reverse --
	----------------------------

	function All_One_Parity_Reverse (Input_Value : in Word) return Word is
		Field : Bit_Field := To_Bit_Field(Input_Value);
		Temp  : Bit_Field := To_Bit_Field(Input_Value);
		Res   : Word;
	begin
		for I in Field'Range loop
			Temp(I) := Field((Field'Last - I));
		end loop;

		Res := To_Word(Temp);
		Res := All_One_Parity(Res);

		Field := To_Bit_Field(Res);

		for I in Field'Range loop
			Temp(I) := Field((Field'Last - I));
		end loop;

		return To_Word(Temp);
	end All_One_Parity_Reverse;

	-------------------------------
	-- Common-Alternation_Parity --
	-------------------------------

	function Common_Alternation_Parity (Input_A, Input_B : in Word) return Word is
		A : Word := not (Input_A xor Input_B);
		B : Word := Shift_Right(A, 1);
		C : Word := Input_A xor Shift_Right(Input_A, 1);
	begin -- Common_Alternation_Parity
		return All_One_Parity (A and B and C);
	end Common_Alternation_Parity;

	----------------------------------
	-- Differential_Probabiltiy_Max --
	----------------------------------

	procedure Differential_Probability_Max
		(Alpha				  : in     Bit_Field;
		 Beta				  : in     Bit_Field;
		 Gamma				  : in out Bit_Field;
		 P					  : in out Bit_Field;
		 Index				  : in     Integer;
		 Output_Differentials : in out Word_Vector.Vector) is
		I         : Bit_Number;
		I_Minus_1 : Bit_Number;
		Max_Size  : Positive := 21;
		-- boolean array access indices
	begin
	-- exit condition, step 4
	if Index = Bit_Field'Length then
		Word_Vector.Append (Output_Differentials, To_Word (Gamma));
		-- call this procedure recursively, step 3
	else
		I := Bit_Number'Val(Index);
		I_Minus_1 := Bit_Number'Val (Index - 1);

		if (Alpha(I_Minus_1) = Beta(I_Minus_1)) and (Beta(I_Minus_1) = Gamma(I_Minus_1)) then
			Gamma(I) := Alpha(I) xor Beta(I) xor Alpha(I_Minus_1);
			if Integer(Word_Vector.Length (Output_Differentials)) < Max_Size then
				Differential_Probability_Max (Alpha, Beta, Gamma, P, Index + 1, Output_Differentials);
			end if;
		elsif (I = Bit_Field'Length - 1) or (Alpha(I) /= Beta(I)) or P(I) then
			-- first case: Gamma(I) := 0
			Gamma(I) := False;
			if Integer(Word_Vector.Length (Output_Differentials)) < Max_Size then
				Differential_Probability_Max (Alpha, Beta, Gamma, P, Index + 1, Output_Differentials);
			end if;

			-- second case: Gamme(I) := 1
			Gamma(I) := True;
			if Integer(Word_Vector.Length (Output_Differentials)) < Max_Size then
				Differential_Probability_Max (Alpha, Beta, Gamma, P, Index + 1, Output_Differentials);
			end if;
		else
			Gamma(I) := Alpha(I);
			if Integer(Word_Vector.Length (Output_Differentials)) < Max_Size then
				Differential_Probability_Max (Alpha, Beta, Gamma, P, Index + 1, Output_Differentials);
			end if;
		end if;
	end if;
	end Differential_Probability_Max;

	-------------------------------
	-- All_Optimal_Differentials --
	-------------------------------

	procedure All_Optimal_Differentials (Delta_A   : in     Word;
										 Delta_B   : in     Word;
										 Delta_Out : in out Word_Vector.Vector) is
		Alpha : Bit_Field := To_Bit_Field(Delta_A);
		Beta  : Bit_Field := To_Bit_Field(Delta_B);
		Gamma : Bit_Field;   -- arbitrary
        P     : Bit_Field;
        Index : Integer   := 1;
      pragma Warnings (Off, Gamma);
      pragma Warnings (Off, P);
    begin
	Word_Vector.Clear (Delta_Out);

        Gamma(0) := Alpha(0) xor Beta(0);
        -- step 1
        P := To_Bit_Field (Common_Alternation_Parity (Delta_A, Delta_B));
        -- step 2

        Differential_Probability_Max (Alpha, Beta, Gamma, P, Index, Delta_Out);

        if Integer (Word_Vector.Length (Delta_Out)) > 1 then
            Word_Vector.Delete_Last (Delta_Out);
        end if;
    end All_Optimal_Differentials;

    ---------------------------------
    -- Optimal_Differential_Output --
    ---------------------------------

    procedure Optimal_Differential_Output
		(Delta_A   : in     Word;
		 Delta_B   : in     Word;
		 Delta_Out : in out Word) is
        r, e, a, p, b : Word;
    begin
        r := Delta_A and 1;
        e := (not (Delta_A xor Delta_B)) and not r;
        a := e and shift_left(e,1) and (Delta_A xor Shift_Left(Delta_A, 1));
        p := All_One_Parity_Reverse(a);
        a := (a or shift_right(a, 1)) and not r;
        b := Shift_Left((a or e), 1);
        Delta_Out := ((Delta_A xor p) and a) or ((Delta_A xor Delta_B xor shift_left(Delta_A, 1)) and not a and b) or (Delta_A and not a and not b);
        Delta_Out := (Delta_Out and not 1) or ((Delta_A xor Delta_B) and 1);
    end Optimal_Differential_Output ;

    ------------------------------
    -- Differential_Probabiltiy --
    ------------------------------

    procedure Differential_Probability 
		(Delta_A     : in     Word;
		 Delta_B     : in     Word;
		 Delta_Out   : in     Word;
		 Probability : in out Conditioned_Float) is
        Differentials_Are_Equal : Word := EQ (Shift_Left(Delta_A, 1), Shift_Left(Delta_B, 1), Shift_Left(Delta_Out, 1));
        Xor_Differentials : Word := Delta_A xor Delta_B xor Delta_Out xor Shift_Left(Delta_B, 1);
        Weight : Integer := 0;
    begin
        ----Debug.Trace("dx-dea.adb::Differential_Probability -> eingaben: Delta_A: " & Word'Image(Delta_A) & " Delta_B:" & Word'Image(Delta_B) & " Delta_Out: " & Word'Image(Delta_A));
        if (Differentials_Are_Equal and Xor_Differentials) /= 0 then
            Probability := 0.0;
        else
            Weight := Compute_Hamming_Weight((not Differentials_Are_Equal) and Mask);
            Probability := 2.0**(-Weight);
        end if;
        --Debug.Trace("dx-dea.adb::Differential_Probability -> WS after calc: " & Conditioned_Float'Image(Probability));
    end Differential_Probability;

    -------------
    -- Compute --
    -------------

    procedure Compute (Bitset    : in Bit_Field;
            		   Position  : in Integer;
            		   Answer    : in out Vector_Of_Sets.Vector) is
        Results : Word_Set.Set;
        Variable_Set : Bit_Field := (others => false);--Bitset;
    begin
		Results.Insert (To_Word (Variable_Set));
		-- added 2#0#

        Variable_Set := Bitset;
        if Variable_Set /= To_Bit_Field (2#0#) then
			Results.Insert (To_Word (Variable_Set));
			-- given slice of modular type added
		end if;

        for N in Position .. Variable_Set'Length - 1 loop
            Variable_Set (Bit_Number'Val(N)) := True;
            Results.Insert (To_Word (Variable_Set));
			-- propagated bit added
        end loop;

		-- check for duplicates before append
		-- slow comparison, but will save much computation time
		declare
			Cursor 		  : Vector_Of_Sets.Cursor;
			Chain  		  : Word_Set.Set;
			Is_Equivalent : Boolean := False;
			use type Ada.Containers.Count_Type;
		begin
			Cursor := Answer.First;
			loop
				exit when not Vector_Of_Sets.Has_Element (Cursor);
				Chain := Vector_Of_Sets.Element (Cursor);
				if Word_Set.Equivalent_Sets (Left => Results, Right => Chain) then
					Is_Equivalent := True;
					exit;
				end if;
				Vector_Of_Sets.Next (Cursor);
			end loop;

			if not Is_Equivalent then
				Answer.Append (Results);
			end if;
		end;
    end Compute ;

    --------------------------
    -- Propagate_Carry_Bits --
    --------------------------

	procedure Propagate_Carry_Bits (Current_Bitset   	 : in     Bit_Field;
									Chains_Of_Carry_Bits : in out Vector_Of_Sets.Vector;
									Maximum 			 : in     Integer := 5) is
		Variable_Set : Bit_Field := (others => false);
		Number_Of_Propagations : Integer := 0;
	begin
		-- begin with the propagation of the most significant bit
		for Position in reverse 0 .. Current_Bitset'Length - 1 loop
            exit when Number_Of_Propagations >= Maximum;

            Variable_Set (Bit_Number'Val(Position)) := False;
            if Current_Bitset (Bit_Number'Val(Position)) then
				Variable_Set (Bit_Number'Val(Position)) := False;
				-- set

                Compute (Variable_Set, Position + 1, Chains_Of_Carry_Bits);
				-- compute

				Number_Of_Propagations := Number_Of_Propagations + 1;
				-- move on
            end if;
        end loop;
	end;

    -----------------------
    -- Recursive_Compute --
    -----------------------

	procedure Recursive_Compute (Chains_Of_Carry_Bits : in     Vector_Of_Sets.Vector;
								 Current_Index        : in     Integer;
								 Current_Value        : in out Word;
								 Answer               : in out Word_Set.Set) is
		Current_Chain : Word_Set.Set := Vector_Of_Sets.Element (Chains_Of_Carry_Bits, Current_Index - 1);
		Chain_Cursor  : Word_Set.Cursor;
	begin
		Chain_Cursor := Current_Chain.First;
		loop
			exit when not Word_Set.Has_Element (Chain_Cursor);

			if Current_Index < Integer (Vector_Of_Sets.Length (Chains_Of_Carry_Bits)) then
				Current_Value := Current_Value XOR Word_Set.Element (Chain_Cursor);
				--Current_Index := Current_Index + 1;

				if Integer (Word_Set.Length (Answer)) < 10_000 then
					Recursive_Compute (Chains_Of_Carry_Bits, Current_Index + 1, Current_Value, Answer);
				else
					exit;
				end if;

				Current_Value := Current_Value XOR Word_Set.Element (Chain_Cursor);
			else
				declare
					Xored_Result : Word := Current_Value XOR Word_Set.Element (Chain_Cursor);
				begin
					if not Word_Set.Contains (Answer, Xored_Result) then
						Word_Set.Insert (Answer, Xored_Result);
					end if;
				end;
			end if;

			Word_Set.Next (Chain_Cursor);
		end loop;
	end;

    -----------------------------------
    -- All_Non_Optimal_Differentials --
    -----------------------------------

    procedure All_Non_Optimal_Differentials 
		(Delta_A     : in     Word;
		 Delta_B     : in     Word;
		 Delta_Out   : in out Word_Vector.Vector) is
        Chains_Of_Carry_Bits    : Vector_Of_Sets.Vector;
		Answer_Set 				: Word_Set.Set;
    begin
		--  return immediately, if no bits can be propagated
		if Delta_A = 2#0# and Delta_B = 2#0# then
			Word_Vector.Append (Delta_Out, 2#0#);
			return;
		end if;

		-- propagate all possible carry bits to the most significant bit
		Propagate_Carry_Bits (To_Bit_Field (Delta_A), Chains_Of_Carry_Bits);
		Propagate_Carry_Bits (To_Bit_Field (Delta_B), Chains_Of_Carry_Bits);

		declare
			Current_Index           : Integer := 1;
			Current_Value           : Word := 2#0#;

			Set_Of_Xored_Carry_Bits : Word_Set.Set;
			Cursor_For_Carry_Bits   : Word_Set.Cursor;
		begin
			Recursive_Compute (Chains_Of_Carry_Bits, Current_Index, Current_Value, Set_Of_Xored_Carry_Bits);
			Cursor_For_Carry_Bits := Set_Of_Xored_Carry_Bits.First;
			loop
				exit when not Word_Set.Has_Element (Cursor_For_Carry_Bits);

				declare
					Differential_Value : Word := Delta_A XOR Delta_B XOR Word_Set.Element (Cursor_For_Carry_Bits);
				begin
					if not Word_Set.Contains (Answer_Set, Differential_Value) then
						Word_Set.Insert (Answer_Set, Differential_Value);
					end if;
				end;

				Word_Set.Next (Cursor_For_Carry_Bits);
			end loop;
		end;

		PQueue.Clear;

		-- compatibility issues (cast to vector)
		declare
			Cursor 		  : Word_Set.Cursor;
		begin
			Cursor := Answer_Set.First;

			loop
				exit when not Word_Set.Has_Element (Cursor);
				declare
					Output_Differential : Word := Word_Set.Element (Cursor);
					Probability         : Conditioned_Float := 1.0;
				begin -- check for impossible outputs (fail-safe)
					Differential_Probability (Delta_A, Delta_B, Output_Differential, Probability);
					declare
						P : Pair := (Output_Differential, Probability);
					begin
						PQueue.Append (P);
					end;
				end;

				Word_Set.Next (Cursor);
			end loop;

			loop  --  return the best xx differentials
				exit when PQueue.Is_Empty;
					declare
						P : Pair := PQueue.Front; -- take front
					begin
						Word_Vector.Append (Delta_Out, P.Element);
						PQueue.Pop; -- pop front
					end;
			end loop;
		end;
    end All_Non_Optimal_Differentials;

    ---------
    -- "<" --
    ---------

    function "<" (Left, Right : Pair) return Boolean is
    begin
        return Left.Priority > Right.Priority;
    end "<";

end DXPL.Support.Differential_Addition;