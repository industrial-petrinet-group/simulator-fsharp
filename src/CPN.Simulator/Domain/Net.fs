namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing the the list of inputs and outputs of a transition.
type TransitionIO = 
    { i: (PlaceId * ArcId) list
      o: (PlaceId * ArcId) list }

/// Type representing the whole Net; it's core is based upon transitions because
/// it's the center of the simulation.
[<StructuredFormatDisplay("{Show}")>]
type Net = Net of Map<TransitionId, TransitionIO>

/// Module implementing Net's operations.
module Net =
    let private random = System.Random()

    /// Returns and empty Net
    let empty = Net Map.empty

    /// Given a Net check if it's empty
    let isEmpty (Net net) = net |> Map.isEmpty
    
    /// Given a Net it returns a list of Transitions ID
    let keyList (Net net) = 
        [] |> Map.foldBack (fun key _ acc -> key :: acc ) net

    /// Given a Net it returns a random ordered list of Transitions ID to 
    /// retrive the information of the Net randomly
    let randomKeyList (Net net) = 
        net
        |> Map.fold (fun acc key _ -> key :: acc ) []
        |> randomizeList
    
    /// Given a TransitionId and a Net it returns the corresponding TransitionIO.
    let find tid (Net net) = net |> Map.find tid

    /// Given a Net, add a pair of TransitionId and TransitionIO.
    let add tid transitionIO (Net net) = Net <| net.Add(tid, transitionIO)

    /// Given a TransitionId and a Net it returns a PlaceId list of it's inputs.
    let inputs tid net =
        let { i = inputs } = net |> find tid

        inputs |> List.map fst
    
    /// Given a TransitionId and a Net it returns a PlaceId list of it's outputs.
    let outputs tid net =
        let { o = outputs } = net |> find tid

        outputs |> List.map fst

    /// Given Places, a transition Id and a Net it returns if the tid is 
    /// available to occur.
    let isEnabled places tid net =
        // FIXME: Due to naive implementation it only checks that a token exist
        net 
        |> inputs tid
        |> List.forall (fun pid -> places |> Place.hasTokens pid)

    /// Given Places and a Net it returns a new Net with only the transitions. 
    /// avaliable to occur.
    let enabledFor places (Net net) =  
        Net <| (net |> Map.filter (fun tid _ -> isEnabled places tid (Net net)))
    

type Net with
    /// Reimplements the way of showing the Net
    member this.Show = 
        let (Net net) = this

        net |> Map.toList