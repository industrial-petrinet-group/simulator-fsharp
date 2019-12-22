namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing the Coloured Petri Net
[<StructuredFormatDisplay("CPN = {Show}")>]
type CPN = CPN of Transitions * Places

/// Module for implementing all CPN's operations
module CPN =
    /// Given a CPN it return the state of it; i.e the list of places with it's 
    /// respective tokens for every place that have at least one.
    let netMarking (CPN (_, places))  =
        places 
        |> Place.placesMarkingAsStringList
        |> List.filter (fun (_, marking) -> marking <> "")

    /// Given a CPN it returns a Net with transitions avaliable to occur.
    let enabled (CPN (transitions, places)) = 
        transitions |> Transition.enabledFor places 
    
    /// Remove the input tokens for the places reached by the enabled transitions.
    let removeInputTokens (enabled, CPN (transitions, places)) =
        let randomKeyList = enabled |> Transition.randomKeyList
           
        Ok (Transition.empty, places)
        |> List.foldBack (fun tid acc ->              
            acc
            >>= fun (occurred, lastPlaces) ->
                match Transition.isEnabled lastPlaces tid enabled with
                    | false -> Ok (occurred, lastPlaces) 
                    | true -> 
                        lastPlaces 
                        |> Place.modifyTokensForList 
                            Place.removeTokens 1 (enabled |> Transition.inputs tid)
                        >>= fun modifiedPlaces -> 
                            let tdata = enabled |> Transition.find tid 
                            Ok (occurred |> Transition.add tid tdata, modifiedPlaces)
        ) randomKeyList
        >>= fun (ocurred, modifiedPlaces) -> 
            Ok (ocurred, CPN (transitions, modifiedPlaces))


    /// Add the output tokens for the places reached by the enabled transitions.
    let addOutputTokens (ocurred, CPN (transitions, places)) =         
        let keyList = ocurred |> Transition.keyList
        
        Ok places
        |> List.foldBack (fun tid acc ->            
            acc 
            >>= fun lastPlaces ->
                lastPlaces 
                |> Place.modifyTokensForList 
                    Place.addTokens 1 (ocurred |> Transition.outputs tid)
        ) keyList
        >>= fun modifiedPlaces -> 
            Ok (ocurred, CPN (transitions, modifiedPlaces))

/// Type representing a way of showing the CPN
type ShowableCPN = 
        { netMarking: (PlaceId * string) list; 
          transitions: (TransitionId * TransitionData) list }

type CPN with
    /// Reimplements the way of showing the CPN
    member this.Show = 
        let (CPN (transitions, _)) = this
        let netMarking = CPN.netMarking this

        { netMarking = netMarking; transitions = transitions.Show }