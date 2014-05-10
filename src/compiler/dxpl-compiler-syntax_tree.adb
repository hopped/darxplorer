with Ada.Strings.Fixed;
with Ada.Text_IO;

package body DXPL.Compiler.Syntax_Tree is

	procedure Process_Item (Item : ASU.Unbounded_String; Success : out Boolean) is

		function Is_Operation (Item : in ASU.Unbounded_String) return Boolean is
			use Ada.Strings.Unbounded;
		begin
			return Item = "Rotate_Left" or Item = "Rotate_Right" or Item = "Shift_Left" or Item = "Shift_Right";
		end Is_Operation;

		function Is_Function_Call (Item : in ASU.Unbounded_String) return Boolean is
		begin
			return Symbol_Table.Has_Element (S_Table, Item, Symbol_Table.t_function) or 
				   Symbol_Table.Has_Element (S_Table, Item, Symbol_Table.t_variable);
		end Is_Function_Call;

		function Is_Operand (Item : in ASU.Unbounded_String) return Boolean is
			use Ada.Strings.Unbounded;
		begin
			if Item = "+" or Item = "-"   or Item = "*"   or Item = ":=" or Item = "mod" 
		                  or Item = "xor" or Item = "and" or Item = "or" or Item = "=>" then
				return False;

			elsif Is_Operation (Item) then
				return False;

			elsif Is_Function_Call (Item) then
				return False;

			else
				return True;
			end if;
		end Is_Operand;

	begin
		if Is_Operand (Item) then
			Operator_Vector.Append (Operator_Stack, Item);
		else
			declare
				Element  : ASU.Unbounded_String := Operator_Vector.Last_Element (Operator_Stack);
				Extended : ASU.Unbounded_String;
			begin
				Operator_Vector.Delete_Last (Operator_Stack);
				declare
					Last_Element : ASU.Unbounded_String := Operator_Vector.Last_Element (Operator_Stack);
					use Ada.Strings.Unbounded;
				begin
					if Is_Operation (Item) then
						Extended := Item & " (" & Last_Element & ", " & Element & ") ";
						Operator_Vector.Delete_Last (Operator_Stack);
					elsif Is_Function_Call (Item) then
						Extended := Item & " (" & Element & ") ";
					else
						Extended := "(" & Last_Element & Item & " " & Element & ") ";
						Operator_Vector.Delete_Last (Operator_Stack);
					end if;
				end;

				Operator_Vector.Append (Operator_Stack, Extended);
			end;
		end if;

		Success := True;
	end Process_Item;

	function Get_Infix_Statement 
		(T : in Binary_Trees.Binary_Tree; Cut : in Boolean := False; Inline : in Boolean := False) return String is
		Last_Element : ASU.Unbounded_String := ASU.Null_Unbounded_String;
		Success      : Boolean := False;
		
		use Ada.Strings.Unbounded;
	begin
		Operator_Vector.Clear (Operator_Stack);
		
		Operand_Tree_Order (T, Success);
		
		Last_Element := Operator_Vector.Last_Element (Operator_Stack);
		if Cut then
			Last_Element := ASU.To_Unbounded_String (ASU.Slice (Last_Element, 2, ASU.Length (Last_Element) - 2));
		end if;
		if not Inline then
			Last_Element := Last_Element & ";";
		end if;

		return ASU.To_String (Last_Element); 
	end Get_Infix_Statement;

	procedure Print_Tree (T : Binary_Trees.Binary_Tree; Message : String := ""; Depth : Natural := 0) is
		use Ada.Text_IO;

		procedure Indent (To : Natural := Depth) is
		begin
			for N in 1 .. Integer (To) loop
				Put ("  ");
			end loop;
		end Indent;

		Former_File : File_Type := Ada.Text_IO.Current_Output;
	begin
		Set_Output (File => Standard_Output);

		if Depth = 0 then
			Put ("Binary tree: " & Message & " ");
		end if;

		if Binary_Trees.Is_Null (T) then
			Put_Line ("(null)");
		else
			if Binary_Trees.Is_Root (T) then
				Put ("(root) ");
			end if;
			Put_Line (":= " & ASU.To_String (Binary_Trees.Item_At (T)));
			if Binary_Trees.Has_Children (T) then
				Indent;
				Put ("L ");
				Print_Tree (Binary_Trees.Left_Child (T),  Depth => Depth + 1);
				Indent;
				Put ("R ");
				Print_Tree (Binary_Trees.Right_Child (T), Depth => Depth + 1);
			end if;
		end if;

		Set_Output (File => Former_File);
	end Print_Tree;

	function Split_Addition (Item : in Binary_Trees.Binary_Tree) return Sum_Type is
		Tmp    : Binary_Trees.Binary_Tree := Item;
		Answer : Sum_Type;
		
		use Ada.Strings.Unbounded;
	begin
		if Binary_Trees.Item_At (Tmp) = ":=" then
			Answer.Sum := Binary_Trees.Item_At (Binary_Trees.Left_Child (Tmp));
			Binary_Trees.Right_Child (Tmp);
		else
			Answer.Sum := Binary_Trees.Item_At (Tmp);
		end if;
		
		declare
			Str_1 : String := Get_Infix_Statement (Binary_Trees.Left_Child  (Tmp), False, True);
			Str_2 : String := Get_Infix_Statement (Binary_Trees.Right_Child (Tmp), False, True);
		begin
			if ASU.Element (ASU.To_Unbounded_String (Str_1), 1) in '0' .. '9' or 
			   ASU.Element (ASU.To_Unbounded_String (Str_2), 1) in '0' .. '9' then
				return Answer;
			else
				Answer.Left_Addend  := ASU.To_Unbounded_String (Str_1);
				Answer.Right_Addend := ASU.To_Unbounded_String (Str_2);
			end if;
		end;
		
		return Answer;
	end Split_Addition;
	
	procedure Count_Addition (T : in Binary_Trees.Binary_Tree; Count_Plus : in out Natural; Depth : in out Natural) is
		use Ada.Strings.Unbounded;
	begin
		if Binary_Trees.Is_Null (T) then
			null;
		elsif Binary_Trees.Item_At (T) = "+" then
			Count_Plus := Natural'Succ (Count_Plus);
		end if;
			
		if Binary_Trees.Has_Children (T) then
			Depth := Natural'Succ (Depth);
			Count_Addition (Binary_Trees.Left_Child  (T), Count_Plus, Depth);
			Count_Addition (Binary_Trees.Right_Child (T), Count_Plus, Depth);
		end if;
	end Count_Addition;
	
--	procedure Set_to_Addition (T : in Binary_Tree; Root : in out Binary_Tree) is
--	begin
--		if Is_Null (T) then
--			null;

--		elsif Item_At (T) = "+" then
--			Root := T;
--		end if;
			
--		if Has_Children (T) then
--			Set_to_Addition (Left_Child (T), Root);
--			Set_to_Addition (Right_Child (T), Root);
--		end if;
--	end Set_to_Addition;
	
	procedure Filter_Addition_Support (T, Root    : in out Binary_Trees.Binary_Tree;
		 							   Branch     : in     Binary_Trees.Child_Branch;
		 							   Tree_Stack : in out Tree_Vector.Vector;
		 							   Count      : in out Natural) is
		-- remove leading spaces
		Str_Count : String := Ada.Strings.Fixed.Delete (Positive'Image (Count), 1, 1);
		
		use Ada.Strings.Unbounded;
	begin
		if Binary_Trees.Is_Null (T) then
			null;

		elsif Binary_Trees.Item_At (T) = "+" then
			declare
				Temp_Tree : Binary_Trees.Binary_Tree;
				Sum       : Sum_Type := Split_Addition (T);
			begin
				-- handle simple single additions
				if Sum.Left_Addend = ASU.Null_Unbounded_String then
					Binary_Trees.Insert (Temp_Tree, ASU.To_Unbounded_String (Get_Infix_Statement (T, False, True)), Binary_Trees.Left);
					Binary_Trees.Swap_Child (Root, Temp_Tree, Branch);

				else
					-- add statement with temporary variable to the stack (seperate subtree)
					declare
						Temp_Tree : Binary_Trees.Binary_Tree := Binary_Trees.Create (T);
					begin
						--Print_Tree (Temp_Tree);
						Binary_Trees.Insert (Temp_Tree, ASU.To_Unbounded_String (":="), Binary_Trees.Right);
						--Print_Tree (Temp_Tree);
						Binary_Trees.Append (Temp_Tree, ASU.To_Unbounded_String ("State.Tmp" & Str_Count), Binary_Trees.Left, Binary_Trees.Left);
						Tree_Vector.Append  (Tree_Stack, Temp_Tree);
						--Ada.text_io.put_line (get_infix_statement(Temp_Tree));
						--Print_Tree (Temp_Tree);
					end;

					-- modify the original tree to contain the correct temporary branches
					declare
						Temp_Tree : Binary_Trees.Binary_Tree;
						Item : ASU.Unbounded_String := ASU.To_Unbounded_String (" Tmp" & Str_Count & ": aliased Word;");
					begin
						-- add new temporary variable to round state defined in 'dxpl-support-state.ads'
						Round_State.Add (R_State, ASU.To_Unbounded_String ("Tmp" & Str_Count), Item);
						Binary_Trees.Insert (Temp_Tree, ASU.To_Unbounded_String ("State.Tmp" & Str_Count & " "), Binary_Trees.Left);
						Binary_Trees.Swap_Child (Root, Temp_Tree, Branch);
						--Print_Tree (Root);
					end;
				end if;
			end;
			
			Count := Natural'Succ (Count);
		end if;
			
		-- recursive call
		if Binary_Trees.Has_Children (T) then
			declare
				Super : Binary_Trees.Binary_Tree := T;
			begin
				-- left branches
				Root := T;
				Binary_Trees.Left_Child (T);
				Filter_Addition_Support (T, Root, Binary_Trees.Left, Tree_Stack, Count);
				-- right branches
				Root := Super;
				Binary_Trees.Right_Child (Super);
				Filter_Addition_Support (Super, Root, Binary_Trees.Right, Tree_Stack, Count);
			end;
		end if;
	end Filter_Addition_Support;

		
	procedure Filter_Addition (T                     : in     Binary_Trees.Binary_Tree;
							   Expression : in out Statement_Vector.Vector) is
		Operand_Tree, Tmp_Tree : Binary_Trees.Binary_Tree := Binary_Trees.Create (T);
		Count        : Natural := 1;
		Tree_Stack   : Tree_Vector.Vector;
	begin		
		Filter_Addition_Support (Operand_Tree, Tmp_Tree, Binary_Trees.Left, Tree_Stack, Count);

		declare
			C : Tree_Vector.Cursor := Tree_Vector.First (Tree_Stack);
		begin
			while Tree_Vector.Has_Element (C) loop
				Statement_Vector.Prepend (Expression, Split_Addition (Tree_Vector.Element (C)));
				Tree_Vector.Next (C);
			end loop;
		end;
		
		--raise Program_Error;
	end Filter_Addition;

end DXPL.Compiler.Syntax_Tree;