namespace CPN.Simulator

open CPN.Simulator.Domain

/// Runtime module in charge of simulate the network.
module Runtime =

    /// return the state of the net; i.e the list of places with it's 
    /// respectives tokens for every place that have tokens
    let netMarking (net: CPN) =
        net
        |> List.map (function Input (p, _), _-> p | Output (_, p), _ -> p )
        |> List.filter (fun p -> p.marking <> [])
        |> List.distinct

    /// return the triggered transitions
    let trigger (net: CPN) =       
        let emptyMap: Map<Transition,Place list> = ([] |> Map.ofList)

        let filteredNet = 
            net |> List.filter (function Input _, _-> true | _ -> false)

        emptyMap
        |> List.foldBack (fun (Input (place, trans), _) acc ->
            match acc.TryFind trans with
            | None -> acc.Add (trans, [place])
            | Some placeList -> acc.Add (trans, place :: placeList)
        ) filteredNet
        |> Map.filter (fun _ placeList -> 
            placeList |> List.forall (fun p -> p.marking <> []))

    /// remove the input tokens from the places involved in trigggering the 
    /// transition.
    let removeInputTokens (toTrigger: Map<Transition, Place list>) net : CPN =
        net 
        |> List.map (function
            | Input (p, t), e when toTrigger.ContainsKey t ->
                let newMarking = 
                    p.marking
                    |> List.tryHead // Given it's only one type there is no need for more logic now
                    |> function
                        | None -> [] // None is needed if more than one transition try to consume the same place
                        | Some {qty = 1} -> []
                        | Some token -> [{token with qty = token.qty - 1}]

                Input ({p with marking = newMarking} , t), e    
            | otherwise -> otherwise)
    
    /// add the output tokens for the places reached by the triggered transition.
    let addOutputTokens (toTrigger: Map<Transition, Place list>) net : CPN =
        net 
        |> List.map (function
            | Output (t, p), e when toTrigger.ContainsKey t ->
                let newMarking = 
                    p.marking
                    |> function
                        | [] -> [SampleNets.unitToken]
                        | token::rest -> {token with qty = token.qty + 1} :: rest
                        // as in it's counterpart; it's simple beacause there is only 1 token

                Output (t, {p with marking = newMarking}), e    
            | otherwise -> otherwise)

    /// make a step in the net
    let step (net: CPN) =
        let toTrigger = trigger net

        match Map.isEmpty toTrigger with
        | true -> false, net
        | false ->
            net 
            |> removeInputTokens toTrigger 
            |> addOutputTokens toTrigger
            |> fun modifiedNet -> true, modifiedNet

        


    
