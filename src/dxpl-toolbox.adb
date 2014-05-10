package body DXPL.Toolbox is

	-----------
    -- Image --
    -----------

	function Image (Technique : in Object) return String is
	--  Returns a string representation of this package. Actually,
	--  it is just the name of the toolbox like Lazy Laura or
	--  Greedy Grete. Often used for debugging or logging aspects.

		pragma Warnings (Off, Technique);
	begin
		return Toolbox_Name;
	end Image;

	-------------------------
	-- Slice_Message_Block --
	-------------------------

	function Slice_Message_Block (Item : in Message_Block) return Word_Array is
	--  Slices a message block, which compromises a complete message (e.g. 256 bit)
	--  into 32 or 64 bit blocks. These blocks are returned as a 'Word_Array'.

		Dummy  : Round_State; 
		Result : Word_Array := Dummy.Message;
		pragma Warnings (Off, Dummy);
		--  use this dummy item to allow the generic use of 'Word_Array' for the variable 'Result'
	begin
		for N in Result'Range loop
			Result (N) := To_Word (Bit_Field (Item (1 + Word'Size * N .. Word'Size * (N + 1))));
		end loop;

		return Result;
	end Slice_Message_Block;

	---------------------------------
	-- Batch_Analyse_With_HW_One --
	---------------------------------

	procedure Batch_Analyse_With_HW_One (Technique : in Object'Class) is
	--  Call 'Analyse' for the given technique with different input messages.
	--  In particular, the state will be modified in such a way, that
	--  all possible input differences with hamming weight 1 are produced
	--  and analyzed.

		Message : Message_Block := (others => False);
		--  Produces a differentials with zeros.
	begin
		for I in reverse Message_Block'Range loop
			Message(I) := True;
			-- Sets the hamming weight to 1

			declare
				Current_State : Round_State;
				--  Round and Stage are set to 1 by default
				pragma Warnings (Off, Current_State);
			begin
				Current_State.Message := Slice_Message_Block (Message);
				Analyse (Technique, Current_State);
			end;

			Message(I) := False;
			-- Resets the hamming weight to 0
		end loop;
	end Batch_Analyse_With_HW_One;

	-------------------------------
	-- Batch_Analyse_With_HW_Two --
	-------------------------------

	procedure Batch_Analyse_With_HW_Two (Technique : in Object'Class) is
	--  Call 'Analyse' for the given technique with different input messages.
	--  In particular, the state will be modified in such a way, that
	--  all possible input differences with hamming weight 2 are produced
	--  and analyzed.

		Message : Message_Block := (others => False);
	begin
		for I in Message_Block'Range loop
			-- set hamming weight to 1
			Message(I) := True;

			for J in reverse I + 1 .. Message_Block'Length loop
				-- set hamming weight to 2
				Message(J) := True;

				declare
					Current_State : Round_State;
					pragma Warnings (Off, Current_State);
				begin
					Current_State.Message := Slice_Message_Block (Message);
					Analyse (Technique, Current_State);
				end;

				-- set hamming weight 2 to 1
				Message(J) := False;
			end loop;

			-- reset hamming weight 1 to 0
			Message(I) := False;
		end loop;
	end Batch_Analyse_With_HW_Two;

	---------------------------------
	-- Batch_Analyse_With_HW_Three --
	---------------------------------

	procedure Batch_Analyse_With_HW_Three (Technique : in Object'Class) is
	--  Call 'Analyse' for the given technique with different input messages.
	--  In particular, the state will be modified in such a way, that
	--  all possible input differences with hamming weight 3 are produced
	--  and analyzed.

		Message : Message_Block := (others => False);
	begin
		for I in Message_Block'Range loop
			-- set hamming weight to 1
			Message(I) := True;

			for J in reverse I + 1 .. Message_Block'Length loop
				-- set hamming weight to 2
				Message(J) := True;

				for K in 1 .. I - 1 loop
					-- set hamming weight to 3
					Message(K) := True;

					declare
						Current_State : Round_State;
					pragma Warnings (Off, Current_State);
					begin
						Current_State.Message := Slice_Message_Block (Message);
						Analyse (Technique, Current_State);
					end;

					-- set hamming weight to 2
					Message(K) := False;
				end loop;

				-- set hamming weight to 1
				Message(J) := False;
			end loop;

			-- set hamming weight to 0
			Message(I) := False;
		end loop;
	end Batch_Analyse_With_HW_Three ;

	--------------------------------
	-- Batch_Analyse_With_HW_Four --
	--------------------------------

	procedure Batch_Analyse_With_HW_Four (Technique : in Object'Class) is
	--  Call 'Analyse' for the given technique with different input messages.
	--  In particular, the state will be modified in such a way, that
	--  all possible input differences with hamming weight 4 are produced
	--  and analyzed.

		Message : Message_Block := (others => False);
	begin
		for I in 3 .. Message_Block'Length - 1 loop
			Message(I) := true;

			for L in 2 .. I - 1 loop
				Message(L) := True;

				for H in 1 .. L - 1 loop
					Message(H) := True;
   
					for J in (I + 1) .. Message_Block'Length loop
						Message(J) := True;

						declare
							Current_State : Round_State;
							pragma Warnings (Off, Current_State);
						begin
							Current_State.Message := Slice_Message_Block (Message);
							Analyse (Technique, Current_State);
						end;

						Message(J) := False;
					end loop;

					Message(H):= False;
				end loop;
   
				Message(L) := False;
			end loop;

			Message(I) := False;
		end loop;
	end Batch_Analyse_With_HW_Four;

	-------------------
	-- Batch_Analyse --
	-------------------

	procedure Batch_Analyse (Technique : in Object; Weight : in Hamming_Weight) is
	--  A simple switch to call the proper batch analyze method

	begin
		case Weight is
			when HW_1 =>
				Batch_Analyse_With_HW_One   (Technique);
			when HW_2 =>
				Batch_Analyse_With_HW_Two   (Technique);
			when HW_3 =>
				Batch_Analyse_With_HW_Three (Technique);
			when HW_4 =>
				Batch_Analyse_With_HW_Four  (Technique);
		end case;
	end Batch_Analyse;
end DXPL.Toolbox;