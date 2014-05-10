package body DXPL.Compiler.Support.Input_Manager is

	function Get_Input return Input_Type is
	begin
		return Input.Current_Input;
	end Get_Input;

	procedure Set_Input (Item : in Input_Type) is
	begin
		Input.Current_Input := Item;
	end Set_Input;

	procedure Set_Input (Item : in Input_Type; Key : in ASU.Unbounded_String) is
		use ASU;
	begin
		Input.Current_Input := Item;
		if Input.Current_Input = MEMORY then
			if Key = ASU.Null_Unbounded_String then
				raise Program_Error;
			end if;
			Input.Current_Stream := Key;
			Input.Current_Vector := Lexical_Map.Element (Input.Lexical_Storage, Input.Current_Stream);
			Input.Current_Cursor := Lexer.Lexical_Vector.Extended_Index'First + 1;
			Input.Old_Symbol     := Lexer.Current_Symbol;
			Lexer.Current_Symbol := Lexer.Lexical_Vector.Element (Input.Current_Vector, Input.Current_Cursor);
		else
			Lexer.Current_Symbol := Input.Old_Symbol;
		end if;
	end Set_Input;

	procedure Add (Key : in ASU.Unbounded_String; Item : in Lexer.Lexical_Vector.Vector) is
	begin
		if Input.Lexical_Storage.Contains (Key) then
			Lexical_Map.Replace (Input.Lexical_Storage, Key, Item);
		else
			Lexical_Map.Insert (Input.Lexical_Storage, Key, Item);
		end if;
	end Add;

	procedure Replace_Dataset (Item : in Lexer.Lexical_Vector.Vector) is
	begin
		Lexical_Map.Replace (Input.Lexical_Storage, Input.Current_Stream, Item);
		Input.Current_Vector := Lexical_Map.Element (Input.Lexical_Storage, Input.Current_Stream);
		Input.Current_Cursor := Lexer.Lexical_Vector.Extended_Index'First + 1;
		Lexer.Current_Symbol := Lexer.Lexical_Vector.Element (Input.Current_Vector, Input.Current_Cursor);
	end Replace_Dataset;
	
	procedure Reset is
	begin
		if Input.Current_Input = MEMORY then
			Input.Current_Cursor := Lexer.Lexical_Vector.Extended_Index'First + 1;
			Lexer.Current_Symbol := Lexer.Lexical_Vector.Element (Input.Current_Vector, Input.Current_Cursor);
		end if;
	end Reset;

	function Has_More_Symbols return Boolean is
	begin
		return Input.Current_Cursor < Lexer.Lexical_Vector.Extended_Index'Val (Lexer.Lexical_Vector.Length (Input.Current_Vector));
	end Has_More_Symbols;
	
	procedure Next_Symbol (Item : in out Lexer.Symbol_Type) is
	begin
		Input.Current_Cursor := Input.Current_Cursor + 1;
		Item := Lexer.Lexical_Vector.Element (Input.Current_Vector, Input.Current_Cursor);
	end Next_Symbol;
	
	procedure Clear is
	begin
		Input.Current_Input := FILE;
		Input.Current_Stream := ASU.Null_Unbounded_String;
		Lexer.Lexical_Vector.Clear (Input.Current_Vector);
		-- Current_Cursor needs not to be cleared
		Lexical_Map.Clear (Input.Lexical_Storage);
		-- Old_Symbol does not need to be cleared
	end Clear;
end DXPL.Compiler.Support.Input_Manager;
