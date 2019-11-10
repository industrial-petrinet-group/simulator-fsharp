namespace CPN.Simulator.Domain

/// Type representing the the list of inputs and outputs of a transition.
type TransitionIO = 
    { i: (PlaceId * ArcId) list
      o: (PlaceId * ArcId) list }

/// Type representing the whole Net; it's core is based upon transitions because
/// it's the center of the simulation.
type Net = Map<TransitionId, TransitionIO>

module Net =
    let private random = System.Random()

    let randomKeyList (net: Net) = 
        net
        |> Map.fold (fun acc key _ -> key :: acc ) []
        |> List.sortBy (fun _ -> random.Next())
    
    let pick (net: Net) keyOrder =
        match keyOrder with
        | [] -> None
        | toPick :: rest -> Some (net |> Map.find toPick, rest)