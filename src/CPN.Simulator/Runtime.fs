namespace CPN.Simulator

open CPN.Simulator.Domain
open CPN.Simulator.Operators

/// Runtime module in charge of simulate the network.
module Runtime =   
    /// Given a CPN net it executes a Step in the simulation
    let step (cpn: CPN) =
        let enabled = CPN.enabled cpn

        match enabled |> Transition.isEmpty with
        | true -> Ok (false, cpn)
        | false ->
            (enabled, cpn) 
            |> CPN.removeInputTokens
            >>= CPN.addOutputTokens
            >>= fun (_ , modifiedCPN) -> Ok (true, modifiedCPN)
    
    /// Make a sequence of all posible Steps
    let rec allSteps (actNet: CPN) = seq {
        match actNet |> step with 
        | Error _ -> yield actNet
        | Ok (false, _) -> yield actNet
        | Ok (true, nextStepNet) ->
            yield actNet
            yield! allSteps nextStepNet
    }