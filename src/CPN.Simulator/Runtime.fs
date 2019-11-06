namespace CPN.Simulator

open CPN.Simulator.Domain

/// Runtime module in charge of simulate the network.
module Runtime =
    let runtime = "f"
    // /// remove the input tokens from the places involved in trigggering the 
    // /// transition.
    // let removeInputTokens (toTrigger: Map<Transition, Place list>) net : CPN =
    //     net 
    //     |> List.map (function
    //         | Input (p, t), e when toTrigger.ContainsKey t ->
    //             let newMarking = 
    //                 p.marking
    //                 |> List.tryHead // Given it's only one type there is no need for more logic now
    //                 |> function
    //                     | None -> [] // None is needed if more than one transition try to consume the same place
    //                     | Some {qty = 1} -> []
    //                     | Some token -> [{token with qty = token.qty - 1}]

    //             Input ({p with marking = newMarking} , t), e    
    //         | otherwise -> otherwise)
    
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

    // /// make a step in the net
    // let step (net: CPN) =
    //     let toTrigger = trigger net

    //     match Map.isEmpty toTrigger with
    //     | true -> false, net
    //     | false ->
    //         net 
    //         |> removeInputTokens toTrigger 
    //         |> addOutputTokens toTrigger
    //         |> fun modifiedNet -> true, modifiedNet
    
    // /// Make a sequence of all posible Steps
    // let rec allSteps (actNet: CPN) = seq {

    //     match actNet |> step with 
    //     | false, _ -> yield actNet
    //     | true, nextStepNet-> 
    //         yield actNet
    //         yield! allSteps nextStepNet
    // }