with Ada.Text_IO;

package body DXPL.Compiler.Support.State is

	procedure Add (Object : in Instance; Key : in ASU.Unbounded_String; Item : in ASU.Unbounded_String) is
		pragma Warnings (Off, Object);
	begin
		if not Strings_Map.Contains (State, Key) then
			Strings_Map.Insert (State, Key, Item);
		end if;
	end Add;
	
	procedure Set_Key_Support (Object : in Instance; Item : in Boolean) is
		pragma Warnings (Off, Object);
	begin
		Keys_Declared := Item;
	end Set_Key_Support;
	
	function Contains (Object : in Instance; Key : in ASU.Unbounded_String) return Boolean is
		pragma Warnings (Off, Object);
	begin
		return Strings_Map.Contains (State, Key);
	end Contains;

	procedure Add_Function (Object : in Instance;
							Key    : in out ASU.Unbounded_String;
							Name   : in String) is
	--  Creates a new function that returns the actual digest variable. If the
	--  initial message length and the digest length are identical, the
	--  newly created function will return 'Message'. Apart from that, 
	--  the variable 'DXPL_Digest', which is part of the round state, will
	--  be returned.
	begin
		ASU.Append (Source => Key, New_Item => "package body DXPL.Support.State is ");
		if Contains (Object, ASU.To_Unbounded_String ("DXPL_Digest")) then
			ASU.Append (Source => Key, New_Item => "procedure Set_Digest (State : in out Round_State; Item : in Word_Array) is ");
			ASU.Append (Source => Key, New_Item => "begin State.DXPL_Digest := Item; end Set_Digest; ");
			ASU.Append (Source => Key, New_Item => "function Get_Digest (State : in Round_State) return Word_Array is ");
			ASU.Append (Source => Key, New_Item => "begin return State.DXPL_Digest; end Get_Digest;" );
		else
			ASU.Append (Source => Key, New_Item => "procedure Set_Digest (State : in out Round_State; Item : in Word_Array) is ");
			ASU.Append (Source => Key, New_Item => "begin State.Message := Item; end Set_Digest; ");
			ASU.Append (Source => Key, New_Item => "function Get_Digest (State : in Round_State) return Word_Array is ");
			ASU.Append (Source => Key, New_Item => "begin return State.Message; end Get_Digest; ");
		end if;
		ASU.Append (Source => Key, New_Item => "function Get_Name return String is begin return ");
		ASU.Append (Source => Key, New_Item => ASCII.Quotation & Name & ASCII.Quotation & "; ");
		ASU.Append (Source => Key, New_Item => "end Get_Name; end DXPL.Support.State; ");
	end Add_Function;
	
	procedure Trace (Object : in Instance; Directory, Filename, Name : in String) is
		pragma Warnings (Off, Object);
		Former_Output        : Ada.Text_IO.File_Type := Ada.Text_IO.Current_Output;
		State_Representation : ASU.Unbounded_String  := ASU.Null_Unbounded_String;
	begin
		--  prefix
		ASU.Append (Source => State_Representation, New_Item => "package DXPL.Support.State is ");
		ASU.Append (Source => State_Representation, New_Item => "type Round_State is new Basic_State with record ");
		
		--  body
		declare
			Cursor : Strings_Map.Cursor   := State.First;
			Item   : ASU.Unbounded_String := ASU.Null_Unbounded_String;
		begin
			while Strings_Map.Has_Element (Cursor) loop
				Item := Strings_Map.Element (State, Strings_Map.Key (Cursor));
				if ASU.Index (Item, "Word_Array") > 0 and ASU.Index (Item, "others") = 0 then
					ASU.Overwrite (Item, ASU.Length (Item), ":= (others => 16#0#); ");
					--  We need to initialize each variable to avoid randomly values.
				end if;
				ASU.Append (Source => State_Representation, New_Item => Item);
				
				Strings_Map.Next (Cursor);
			end loop;
		end;
		
		--  handle keys
		if not Keys_Declared then
			ASU.Append (Source => State_Representation, New_Item => "key : word_array_4; ");
		end if;
		
		--  suffix
		ASU.Append (Source => State_Representation, New_Item => "end record; ");
		ASU.Append (Source => State_Representation, New_Item => "function Get_Name return String; ");
		ASU.Append (Source => State_Representation, New_Item => "function Get_Digest (State : in Round_State) return Word_Array; ");
		ASU.Append (Source => State_Representation, New_Item => "procedure Set_Digest (State : in out Round_State; Item : in Word_Array); ");
		ASU.Append (Source => State_Representation, New_Item => "end DXPL.Support.State; ");
		
		--  write .ads
		declare
			File_State : Ada.Text_IO.File_Type;
			pragma Warnings (Off, File_State);
		begin
			Ada.Text_IO.Create 
				(File => File_State, 
				 Mode => Ada.Text_IO.Out_File, 
				 Name => Directory & "/" & Filename & ".ads");
			Ada.Text_IO.Set_Output (File => File_State);
			Ada.Text_IO.Put_Line   (ASU.To_String (State_Representation));
			Ada.Text_IO.Set_Output (File => Former_Output);
			Ada.Text_IO.Close      (File => File_State);
		end;
		
		--  create .adb
		ASU.Delete (State_Representation, Positive'First, ASU.Length (State_Representation));
		Add_Function (Object, State_Representation, Name);
		declare
			File_State : Ada.Text_IO.File_Type;
			pragma Warnings (Off, File_State);
		begin
			Ada.Text_IO.Create 
				(File => File_State, 
				 Mode => Ada.Text_IO.Out_File, 
				 Name => Directory & "/" & Filename & ".adb");
			Ada.Text_IO.Set_Output (File => File_State);
			Ada.Text_IO.Put_Line   (ASU.To_String (State_Representation));
			Ada.Text_IO.Set_Output (File => Former_Output);
			Ada.Text_IO.Close      (File => File_State);
		end;
		
		--  clear all states
		Strings_Map.Clear (State);
	end Trace;
end DXPL.Compiler.Support.State;
