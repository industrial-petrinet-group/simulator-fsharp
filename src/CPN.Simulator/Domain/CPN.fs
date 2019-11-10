namespace CPN.Simulator.Domain

/// Type representing the the list of inputs and outputs of a transition.
type TransitionIO = 
    { i: (PlaceId * ArcId) list
      o: (PlaceId * ArcId) list }

/// Type representing the whole Net; it's core is based upon transitions because
/// it's the center of the simulation.
type Net = Map<TransitionId, TransitionIO>

/// Type representing the Coloured Petri Net
type CPN = 
    | CPN of Net * (Places * Transitions * Arcs)

/// Module for implementing all CPN's operations
module CPN = 
    /// Given a CPN it return the state of it; i.e the list of places with it's 
    /// respective tokens for every place that have at least one.
    let netMarking (CPN (_, (places, _, _)))  =
        places
        |> Map.toList
        |> List.map fst
        |> List.distinct
        |> Place.markingsAsStringList places
        |> List.filter (fun (_, marking) -> marking <> "")

    /// Given a transition Id and It's Transition IO it returns if it's available
    /// for triggering.
    let isTriggerable places _ {i = inputs} =
        // FIXME: Due to naive implementation it only checks that a token exist
        inputs
        |> List.map fst
        |> List.forall (fun pid -> 
            places 
            |> Map.tryFind pid 
            |> function 
                | None -> false
                | Some placeData -> placeData.marking <> [])

    /// Given a CPN it return the transitions avaliable to be triggered.
    let toTrigger (CPN (net, (places, _, _))) : Net =  
        net |> Map.filter (isTriggerable places)