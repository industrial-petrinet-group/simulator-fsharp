namespace CPN.Simulator

open CPN.Simulator.Domain
open CPN.Simulator.Operators

/// Runtime module in charge of simulate the network.
module Runtime =

    let private modifyTokensForTriggered (toTrigger, places) modifyFunc =
        let randomKeyList = toTrigger |> Net.randomKeyList
        
        Ok (Map.empty, places)
        |> List.foldBack (fun tid acc ->
            let actTriggered = toTrigger |> Map.find tid
            
            acc 
            >>= fun (newToTrigger, newPlaces) ->
                match CPN.isTriggerable places tid actTriggered with
                | false -> Ok (newToTrigger, newPlaces)
                | true -> 
                    let { i = inputs } = actTriggered
                    
                    let partialModifyFunc = modifyFunc 1 "()" SampleNets.unitColour
                    
                    let resNewPlaces =
                        newPlaces 
                        |> Place.modifyTokensForList 
                            partialModifyFunc (inputs |> List.map fst)
                    
                    resNewPlaces >>= fun actNewPlaces -> 
                        Ok (newToTrigger.Add(tid, actTriggered), actNewPlaces)

        ) randomKeyList

    /// remove the input tokens from the places involved in trigggering the 
    /// transition.    
    let removeInputTokens (toTrigger, places) =
       modifyTokensForTriggered (toTrigger, places) Place.removeTokens

    /// add the output tokens for the places reached by the triggered transition.
    let addOutputTokens (toTrigger, places) = 
        modifyTokensForTriggered (toTrigger, places) Place.addTokens

    
    /// Given a CPN net it executes a Step in the simulation
    let step (cpn: CPN) =
        let toTrigger = CPN.toTrigger cpn
        let (CPN (net, (places, transitions, arcs))) = cpn 

        match toTrigger |> Map.isEmpty with
        | true -> Ok (false, cpn)
        | false ->
            (toTrigger, places) 
            |> removeInputTokens
            >>= addOutputTokens
            >>= fun (_ , newPlaces) ->
                Ok (true, (CPN (net, (newPlaces, transitions, arcs))))
    
    /// Make a sequence of all posible Steps
    let rec allSteps (actNet: CPN) = seq {
        match actNet |> step with 
        | Error _ -> yield actNet
        | Ok (false, _) -> yield actNet
        | Ok (true, nextStepNet) ->
            yield actNet
            yield! allSteps nextStepNet
    }