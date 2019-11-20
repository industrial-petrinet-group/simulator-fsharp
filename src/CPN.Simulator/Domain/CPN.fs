namespace CPN.Simulator.Domain

/// Type representing the Coloured Petri Net
[<StructuredFormatDisplay("CPN = {Show}")>]
type CPN = CPN of Net * (Places * Transitions * Arcs)


/// Module for implementing all CPN's operations
module CPN =
    // TODO: Remove the opening of types; moving logic towards every type when
    // it's convinient
    /// Given a CPN it return the state of it; i.e the list of places with it's 
    /// respective tokens for every place that have at least one.
    let netMarking (CPN (_, (places, _, _)))  =
        places 
        |> Place.placesMarkingAsStringList
        |> List.filter (fun (_, marking) -> marking <> "")

    /// Given a transition Id and It's Transition IO it returns if it's available
    /// to occur.
    let isEnabled (Places places) _ {i = inputs} =
        // FIXME: Due to naive implementation it only checks that a token exist
        inputs
        |> List.map fst
        |> List.forall (fun pid -> 
            places 
            |> Map.tryFind pid 
            |> function 
                // Check if it could be abstracted, already present in Places!
                | None -> false
                | Some placeData -> not (placeData.marking |> MultiSet.isEmpty))

    /// Given a CPN it returns a Net with transitions avaliable to occur.
    let enabled (CPN (Net net, (places, _, _))) : Net =  
        Net <| (net |> Map.filter (isEnabled places))

/// Type representing a way of showing the CPN
type ShowableCPN = 
        { netMarking: (PlaceId * string) list; 
          net: (TransitionId * TransitionIO) list}

type CPN with
    /// Reimplements the way of showing th CPN
    member this.Show = 
        let (CPN (Net net, _)) = this
        let netMarking = CPN.netMarking this

        {netMarking = netMarking; net = net |> Map.toList}