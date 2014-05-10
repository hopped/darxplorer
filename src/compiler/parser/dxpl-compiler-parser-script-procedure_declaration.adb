separate (DXPL.Compiler.Parser.Script)

procedure Procedure_Declaration (DXPL_Support : in out DXPL_Support_Type) is
--  procedure_declaration ::=
--    procedure <identifier> is | procedure <identifier> 
--    "(" <procedure_parameter_declaration> ")" is

	procedure Handle_DXPL_Process is
	begin
		Current_Procedure_Name := Lexer.Current_Symbol.Value;
		
		Match (Z_IDENTIFIER);
		if Lexer.Current_Symbol.Lex_Element = D_LEFT_PARENTHESIS then
		--  work through parameters
			Match (D_LEFT_PARENTHESIS);
			Procedure_Parameter_Declaration;
			Match (D_RIGHT_PARENTHESIS);
		end if;
		Match (R_IS);

		declare
			Item : Unbounded_String;
		begin
			Append (Source => Item, New_Item => "(State : access Round_State) return access Word is ");
			loop --  declaration of local variables
				exit when Lexer.Current_Symbol.Lex_Element = R_BEGIN;
				Debug.Trace ("DXPL_PROCESS VARIABLES DECLARATION: ENTER");
				Variables_Declaration (Item);
			end loop;

			Match_and_Append (R_BEGIN, Item);
			Process_Procedure.Set_Parameter (Item);
		end;

		declare
			Annotation_Object : Annotation_Manager.Instance;
		begin
			Statements (Annotation_Object);
		end;

		Match (R_END);
		Match_Lenient (Z_IDENTIFIER);
		Match (D_SEMI_COLON); 
	end Handle_DXPL_Process;

	procedure Handle_Initialize_And_Finalize is
		
		procedure Inner_Loop is
		--  loops until 'end' is reached
		begin
			loop
				exit when Lexer.Current_Symbol.Lex_Element = R_END;
				if Lexer.Current_Symbol.Lex_Type = TYPE_DARX_IDENTIFIER then
					Match (Z_IDENTIFIER);
					Match (D_DOT);
				end if;

				if Round_State.Contains (R_State, Lexer.Current_Symbol.Value) then
					Trace ("State.");
				end if;
				Trace;
				Next_Symbol;
			end loop;
		end Inner_Loop;
	begin
		Match (D_LEFT_PARENTHESIS);
		Procedure_Parameter_Declaration;
		Match (D_RIGHT_PARENTHESIS);
		Match (R_IS);

		declare
			Parameter : Unbounded_String;
		begin
			Append (Source => Parameter, New_Item => "(State : access Round_State) is ");
			loop --  declaration of local variables
				exit when Lexer.Current_Symbol.Lex_Element = R_BEGIN;
				Debug.Trace ("INITIALIZE/FINALIZE VARIABLES DECLARATION: ENTER");
				Variables_Declaration (Parameter);
			end loop;

			Match_and_Append (R_BEGIN, Parameter);
			Trace (To_String (Parameter));
			
			Inner_Loop;
			Match_and_Trace (R_END);
			loop --  reads over 'end loop'
				exit when not (Lexer.Current_Symbol.Lex_Element = R_LOOP);
				Inner_Loop;
				Match_and_Trace (R_END);
			end loop;
		end;

		Match_Lenient (Z_IDENTIFIER);
		Match_and_Trace (D_SEMI_COLON);
	end Handle_Initialize_And_Finalize;

	procedure Handle_Common_Procedures is
		Elements : Lexer.Lexical_Vector.Vector;
		Key : Unbounded_String := Lexer.Current_Symbol.Value;
	begin
		Symbol_Table.Push (S_Table, Key, Symbol_Table.t_procedure);
		Match (Z_IDENTIFIER);
		
		loop
			exit when Lexer.Current_Symbol.Lex_Element = R_END;
			Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
			Next_Symbol;
		end loop;

		--  end <identifier> ";"
		Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
		Match (R_END);
		Match_Lenient (Z_IDENTIFIER);
		Lexer.Lexical_Vector.Append (Elements, Lexer.Current_Symbol);
		Match (D_SEMI_COLON);

		--  store vector in map
		Input_Manager.Add (Key, Elements);
	end Handle_Common_Procedures;

begin
	Match (R_PROCEDURE);

	case Lexer.Current_Symbol.Lex_Type is
		when TYPE_DARX_PROCESS =>
			if not DXPL_Support.Has_Process then
				DXPL_Support.Has_Process := True;
				Handle_DXPL_Process;
			else
				Error (DXPL_Procedure_Exception'Identity,
				  "More than one 'DXPL_Process' procedure was found, but " &
				  "only one is allowed. Please revise your code.");
			end if;

		when TYPE_DARX_INITIALIZE =>
			if not DXPL_Support.Has_Initialize then
				DXPL_Support.Has_Initialize := True;
				Trace ("procedure ");
				Current_Procedure_Name := Lexer.Current_Symbol.Value;
				Match (Z_IDENTIFIER);
				Trace ("Initialize ");
				Handle_Initialize_And_Finalize;
			else
				Error (DXPL_Procedure_Exception'Identity,
				  "More than one 'DXPL_Initialize' procedure was found, but " &
				  "only one is allowed. Please revise your code.");
			end if;

		when TYPE_DARX_FINALIZE =>
			if not DXPL_Support.Has_Finalize then
				DXPL_Support.Has_Finalize := True;
				Trace ("procedure ");
				Current_Procedure_Name := Lexer.Current_Symbol.Value;
				Match (Z_IDENTIFIER);
				Trace ("Finalize ");
				Handle_Initialize_And_Finalize;
			else
				Error (DXPL_Procedure_Exception'Identity,
				  "More than one 'DXPL_Finalize' procedure was found, but " &
				  "only one is allowed. Please revise your code.");
			end if;

		--  handle local defined procedures in addition to a 'DXPL' procedure
		when others =>
			Handle_Common_Procedures;
	end case;
end Procedure_Declaration;
