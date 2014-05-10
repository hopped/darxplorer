with DXPL.Support.State;  use DXPL.Support.State;

package body DXPL.Toolbox.Graph is
	package Support_State renames DXPL.Support.State;

	-------------
	-- Compute --
	-------------

	procedure Compute (Differentials_Graph : in out Priority_Graph.Graph_Instance;
					   Current_Vertex      : in out Directed_Graph.Vertex'Class;
					   Current_State       : in out Round_State) is
		Modified_Value : access Word;
	begin
		State_Alias := Current_State;

		Current_State.Output_Candidates := Word_Vector.Empty_Vector;
		-- fail-safe to avoid loop over old values, if the next 
		-- statement won't modify the variable output_candidates

		Modified_Value := Module_Sequence (Current_State.Current_Stage)(State_Alias'Access);
		-- Compute the current statement represented by a procedure

		State_Alias.Global_Probability := State_Alias.Global_Probability * State_Alias.Local_Probability;
		-- TODO (hoppe) ist noch falsch...was ist mit der rundenwahrscheinlichkeit?
		-- unterscheide zwischen WS für das statement, WS für die runde und WS insgesamt im pfad

		if Modified_Value /= null then
		-- Process each 'Word' that has changed in a new branch
			declare
				Cursor : Word_Vector.Cursor := State_Alias.Output_Candidates.First;
			begin 
				while Word_Vector.Has_Element (Cursor) loop
					declare
						Child_Node   : Round_State;
						Child_Vertex : Directed_Graph.Vertex;
						pragma Warnings (Off, Child_Vertex);
					begin -- Create new vertex for current state
						Modified_Value.all := Word_Vector.Element (Cursor);
						Child_Node := State_Alias; -- make static reference
						
						--Debug.Show_Candidate (Child_Node);
						
						Priority_Graph.Create_Vertex (Differentials_Graph, Child_Vertex, Child_Node);
						Priority_Graph.Create_Connection
							(Differentials_Graph, Conditional_Arc, State_Alias.Global_Probability, Current_Vertex, Child_Vertex);
					end;

					Word_Vector.Next (Cursor);
				end loop;

				-- Return new vertex with optimal probability
				Current_Vertex := Priority_Graph.Dequeue_Next_Best_Node;
				State_Alias    := Directed_Graph.Item (Current_Vertex);
			end;
		else
			declare
				Child_Node   : Round_State := State_Alias;
				-- makes a static reference
				Child_Vertex : Directed_Graph.Vertex;
				pragma Warnings (Off, Child_Vertex);
			begin -- Create new vertex for current state
				if State_Alias.Round_Offset > Round_Threshold then
					Debug.Put_Line ("Round" & Rounds'Image (State_Alias.Round_Offset) & " reached ... still processing.");
					Round_Threshold := State_Alias.Round_Offset;
				end if;
				
				Priority_Graph.Create_Vertex (Differentials_Graph, Child_Vertex, Child_Node);
				Priority_Graph.Create_Connection
					(Differentials_Graph, Conditional_Arc, State_Alias.Global_Probability, Current_Vertex, Child_Vertex);
				Current_Vertex := Directed_Graph.Vertex'Class (Child_Vertex);
			end;
		end if;

		-- Update to the next stage and eventually to the next round
		if State_Alias.Current_Stage < Round_Characteristic.Module_Offset'Last then
			--Debug.Show_Stage (State_Alias);
			State_Alias.Current_Stage := State_Alias.Current_Stage + 1;
		else
			--Debug.Show_Round (State_Alias, Conditioned_Float'Last);
			State_Alias.Current_Stage := Round_Characteristic.Module_Offset'First;
			if State_Alias.Round_Offset < Rounds'Last then
				State_Alias.Round_Offset := State_Alias.Round_Offset + 1;
			end if;
		end if;

		Current_State := State_Alias;
	end Compute;

	-------------
	-- Analyse --
	-------------

	procedure Analyse (Technique     : in     Instance;
					   Current_State : in out Round_State) is
		Root_Vertex : Directed_Graph.Vertex;
		pragma Warnings (Off, Technique);
    begin
		State_Alias := Current_State;

		-- Initialize, before the first stage will be processed
		Initialize (State_Alias'Access);
		
		-- Create the root of the underlying graph-based structure
		Priority_Graph.Create_Vertex (Forward_Graph, Root_Vertex, State_Alias);

		loop -- Iterative implementation of the recursive computation
			exit when State_Alias.Global_Probability < Toolbox_Threshold or 
				(State_Alias.Round_Offset = Rounds'Last and State_Alias.Current_Stage = Module_Offset'Last);
			Compute (Forward_Graph, Root_Vertex, State_Alias);
		end loop;

		-- Finalization occurs only, if the last round and the last stage were processed
		if State_Alias.Round_Offset = Rounds'Last and State_Alias.Current_Stage = Module_Offset'Last then
			Compute (Forward_Graph, Root_Vertex, State_Alias);
			State_Alias.Current_Stage := Module_Offset'Last;
			--  Computes the last stage of the last round
			Priority_Graph.Add_Last (State_Alias.Global_Probability, Root_Vertex);
			--  Adds the last result to the priority queue. It is indifferent, if
			--  the probability is worse than other states in the queue, because
			--  we could analyzed the whole cipher!
			Finalize (State_Alias'Access);
		end if;
		Logger.Graph_Based (Support_State.Get_Name, Image (Technique), Root_Vertex, Positive (Module_Offset'Last));

		Current_State := State_Alias;
		Priority_Graph.Clear (Forward_Graph);
		--  Resets the graph for next iteration
	end Analyse;

end DXPL.Toolbox.Graph;
