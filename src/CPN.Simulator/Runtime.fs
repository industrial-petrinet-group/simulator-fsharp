namespace CPN.Simulator

open CPN.Simulator.Domain

/// Runtime module in charge of simulate the network.
module Runtime =
    /// remove the input tokens from the places involved in trigggering the 
    /// transition.
    // TODO: implement it with the new trigger type
    let x = ""
    // let removeInputTokens (toTrigger: Trigger) net =
    //     let (CPN (places, transitions, arcs)) = net 
    //     let triggered = []
    //     triggered

        // (places, triggered)
        // |> List.foldBack (fun arc (actPlaces, triggered) ->
        //     match arc with
        //     | Input (_pid, tid), _exp ->
        //         match toTrigger |> Map.tryFind tid with
        //         | None -> actPlaces, triggered
        //         | Some pids ->
        //             match pids |> List.forall (Place.hasTokens actPlaces) with
        //             | false -> actPlaces, triggered
        //             | true ->
        //                 let newPlaces = Place.removeOneFrom actPlaces pids

        //                 newPlaces, toTrigger                        
                            

        //         Input ({p with marking = newMarking} , t), e    
        //     | _otherwise -> net, triggered) arcs
    
    // /// add the output tokens for the places reached by the triggered transition.
    // let addOutputTokens (toTrigger: Map<Transition, Place list>) net : CPN =
    //     net 
    //     |> List.map (function
    //         | Output (t, p), e when toTrigger.ContainsKey t ->
    //             let newMarking = 
    //                 p.marking
    //                 |> function
    //                     | [] -> [SampleNets.unitToken]
    //                     | token::rest -> {token with qty = token.qty + 1} :: rest
    //                     // as in it's counterpart; it's simple beacause there is only 1 token

    //             Output (t, {p with marking = newMarking}), e    
    //         | otherwise -> otherwise)

    /// Given a CPN net it executes a Step in the simulation
    // let step (net: CPN) =
    //     let toTrigger = CPN.toTrigger net

    //     match toTrigger |> Map.isEmpty with
    //     | true -> false, net
    //     | false ->
    //         net 
    //         |> removeInputTokens toTrigger 
            //|> addOutputTokens toTrigger
            //|> fun modifiedNet -> true, modifiedNet
    
    // /// Make a sequence of all posible Steps
    // let rec allSteps (actNet: CPN) = seq {

    //     match actNet |> step with 
    //     | false, _ -> yield actNet
    //     | true, nextStepNet-> 
    //         yield actNet
    //         yield! allSteps nextStepNet
    // }