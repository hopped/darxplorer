separate (DXPL.Compiler.Parser.Script.Statements)

procedure Procedure_Call is
--  Procedure_Call ::=
--    <identifier> "(" <expression> { "," <expression> } ")"
	Tree           : Binary_Tree;
	Procedure_Name : Unbounded_String := Lexer.Current_Symbol.Value;
begin
	Current_Procedure_Name := Lexer.Current_Symbol.Value;

	--  <identifier>
	Match (Z_IDENTIFIER);
	--  "("
	Match (D_LEFT_PARENTHESIS);

	--  <expression> { "," <expression> }
	Tree := Expression;
	Parameter_Manager.Add_Argument (To_Unbounded_String (Syntax_Tree.Get_Infix_Statement (Tree, False, True)));
	while Lexer.Current_Symbol.Lex_Element = D_COMMA loop
		Match (D_COMMA);
		Tree := Expression;
		Parameter_Manager.Add_Argument (To_Unbounded_String (Syntax_Tree.Get_Infix_Statement (Tree, False, True)));
	end loop;
	
	declare
		Item : Unbounded_String := Null_Unbounded_String;
	begin
		--  ")"
		Match (D_RIGHT_PARENTHESIS);
		
		--  switch input to MEMORY
		Input_Manager.Set_Input (MEMORY, Procedure_Name);
		
		Match (D_LEFT_PARENTHESIS);
		Procedure_Parameter_Declaration;
		Match (D_RIGHT_PARENTHESIS);
		Match (R_IS);

		loop --  declaration of local variables
			exit when Lexer.Current_Symbol.Lex_Element = R_BEGIN;
			Debug.Trace ("PROCEDURE_CALL VARIABLES DECLARATION: ENTER");
			Variables_Declaration (Item);
		end loop;

		Match (R_BEGIN);

		--  (1) Parameter passing
		--
		--  computes association of arguments and parameters
		--  to support the generation of functions containing
		--  statements like 'parameter := argument;'
		Trace (Process_Procedure.Get_Next_Declaration);
		Trace (Parameter_Manager.Match_Handover (Procedure_Name));

		Trace ("State.Local_Probability := Conditioned_Float'Last; ");
		Trace ("return null; ");
		Trace ("end; ");
		Trace (" " & ASCII.CR);

		--  (2) Process Statements
		declare
			Annotation_Object : Annotation_Manager.Instance;
		begin
			Statements (Annotation_Object);
		end;

		--  (3) Re-assign modified values to arguments
		--
		--  computes association of arguments and parameters
		--  to support the generation of functions containing
		--  statements like 'argument := parameter;'
		Trace (Process_Procedure.Get_Next_Declaration);
		Trace (Parameter_Manager.Match_Return (Procedure_Name));

		Trace ("State.Local_Probability := Conditioned_Float'Last; ");
		Trace ("return null; ");
		--  no "end" here!

		Match (R_END);
		Match_Lenient (Z_IDENTIFIER);
		Match (D_SEMI_COLON);
		
		--  reset input to FILE and clear list of arguments
		Input_Manager.Set_Input (FILE);
		Parameter_Manager.Reset_Arguments;
	end;
end Procedure_Call;
