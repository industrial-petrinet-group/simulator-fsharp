namespace CPN.Simulator.Domain

open System.Text.RegularExpressions

/// Type representing the Coloured Petri Net
[<StructuredFormatDisplay("CPN = {Show}")>]
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

    /// Given a CPN it returns a Net with transitions avaliable to be triggered.
    let toTrigger (CPN (net, (places, _, _))) : Net =  
        net |> Map.filter (isTriggerable places)

/// Type representing a way of showing the CPN
type ShowableCPN = 
        { netMarking: (PlaceId * string) list; 
          net: (TransitionId * TransitionIO) list}

type CPN with
    /// Reimplements the way of showing th CPN
    member this.Show = 
        let (CPN (net, _)) = this
        let netMarking = CPN.netMarking this

        {netMarking = netMarking; net = net |> Map.toList}