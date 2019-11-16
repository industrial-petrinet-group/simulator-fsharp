namespace CPN.Simulator

open CPN.Simulator.Domain
open CPN.Simulator.Operators

/// Runtime module in charge of simulate the network.
module Runtime =
    /// remove the input tokens from the places involved in trigggering the 
    /// transition.    
    let removeInputTokens (enabled, places) =
        let randomKeyList = enabled |> Net.randomKeyList
        
        Ok (Map.empty, places)
        |> List.foldBack (fun tid acc ->
            let actEnabled = enabled |> Map.find tid
            
            acc 
            >>= fun (occurred, lastPlaces) ->
                match CPN.isEnabled lastPlaces tid actEnabled with
                | false -> Ok (occurred, lastPlaces)
                | true -> 
                    let { i = inputs } = actEnabled
                                        
                    let resModifiedPlaces =
                        lastPlaces 
                        |> Place.modifyTokensForList 
                            Place.removeTokens 1 (inputs |> List.map fst)
                    
                    resModifiedPlaces >>= fun modifiedPlaces -> 
                        Ok (occurred.Add(tid, actEnabled), modifiedPlaces)

        ) randomKeyList

    /// add the output tokens for the places reached by the triggered transition.
    let addOutputTokens (ocurred, places) =         
        Ok places
        |> Map.foldBack (fun _ actOcurred acc ->            
            acc 
            >>= fun lastPlaces ->
                let { o = outputs } = actOcurred

                let resModifiedPlaces =
                    lastPlaces 
                    |> Place.modifyTokensForList 
                        Place.addTokens 1 (outputs |> List.map fst)
                
                resModifiedPlaces 
        ) ocurred
        >>= fun modifiedPlaces -> Ok (ocurred, modifiedPlaces)
    
    /// Given a CPN net it executes a Step in the simulation
    let step (cpn: CPN) =
        let enabled = CPN.enabled cpn
        let (CPN (net, (places, transitions, arcs))) = cpn 

        match enabled |> Map.isEmpty with
        | true -> Ok (false, cpn)
        | false ->
            (enabled, places) 
            |> removeInputTokens
            >>= addOutputTokens
            >>= fun (_ , modifiedPlaces) ->
                Ok (true, (CPN (net, (modifiedPlaces, transitions, arcs))))
    
    /// Make a sequence of all posible Steps
    let rec allSteps (actNet: CPN) = seq {
        match actNet |> step with 
        | Error _ -> yield actNet
        | Ok (false, _) -> yield actNet
        | Ok (true, nextStepNet) ->
            yield actNet
            yield! allSteps nextStepNet
    }