namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing the Coloured Petri Net
[<StructuredFormatDisplay("CPN = {Show}")>]
type CPN = CPN of Net * (Places * Transitions * Arcs * Declarations)


/// Module for implementing all CPN's operations
module CPN =
    // TODO: Remove the opening of types; moving logic towards every type when
    // it's convinient
    /// Given a CPN it return the state of it; i.e the list of places with it's 
    /// respective tokens for every place that have at least one.
    let netMarking (CPN (_, (places, _, _, _)))  =
        places 
        |> Place.placesMarkingAsStringList
        |> List.filter (fun (_, marking) -> marking <> "")

    /// Given a CPN it returns a Net with transitions avaliable to occur.
    let enabled (CPN (net, (places, _, _, _))) = net |> Net.enabledFor places 
    
    /// Remove the input tokens for the places reached by the enabled transitions.
    let removeInputTokens (enabled, CPN (net, (places, transitions, arcs, declarations))) =
        let randomKeyList = enabled |> Net.randomKeyList
           
        Ok (Net.empty, places)
        |> List.foldBack (fun tid acc ->              
            acc
            >>= fun (occurred, lastPlaces) ->
                match Net.isEnabled lastPlaces tid enabled with
                    | false -> Ok (occurred, lastPlaces) 
                    | true -> 
                        lastPlaces 
                        |> Place.modifyTokensForList 
                            Place.removeTokens 1 (enabled |> Net.inputs tid)
                        >>= fun modifiedPlaces -> 
                            let tio = enabled |> Net.find tid 
                            Ok (occurred |> Net.add tid tio, modifiedPlaces)
        ) randomKeyList
        >>= fun (ocurred, modifiedPlaces) -> 
            Ok (ocurred, CPN (net, (modifiedPlaces, transitions, arcs, declarations)))


    /// Add the output tokens for the places reached by the enabled transitions.
    let addOutputTokens (ocurred, CPN (net, (places, transitions, arcs, declarations))) =         
        let keyList = ocurred |> Net.keyList
        
        Ok places
        |> List.foldBack (fun tid acc ->            
            acc 
            >>= fun lastPlaces ->
                lastPlaces 
                |> Place.modifyTokensForList 
                    Place.addTokens 1 (ocurred |> Net.outputs tid)
        ) keyList
        >>= fun modifiedPlaces -> 
            Ok (ocurred, CPN (net, (modifiedPlaces, transitions, arcs, declarations)))

/// Type representing a way of showing the CPN
type ShowableCPN = 
        { netMarking: (PlaceId * string) list; 
          net: (TransitionId * TransitionIO) list }

type CPN with
    /// Reimplements the way of showing the CPN
    member this.Show = 
        let (CPN (net, _)) = this
        let netMarking = CPN.netMarking this

        { netMarking = netMarking; net = net.Show }