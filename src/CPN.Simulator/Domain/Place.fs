namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing a Place Id
type PlaceId = P of int

/// Type representing a Place Data
type PlaceData = 
    { name: string
      colour: ColorSet 
      marking: MultiSet list }

/// Type representing a collection of places
type Places = Map<PlaceId, PlaceData>

/// Type representing Place Errors
type PlaceErrors =
    | UnexectedError of msg: string
    | InexistenPid of pid: PlaceId * places: Places
    | InsufficientTokens of pid: PlaceId * places: Places

/// Module implementing Place's operations.
module Place =
    /// Given Places it returns new Places with it's multiset values reduced
    let reduceMarking places =
        // TODO
        places

    /// Given Places and a pid it return if it has at least one token
    let hasTokens places pid =
        match places |> Map.tryFind pid with
        | None -> false
        | Some placeData -> placeData.marking <> []  

    /// Given a PlaceData it return it's marking parsed as a string.
    let markingAsString placeData =
        MultiSet.listAsString placeData.marking
    
    /// Given a Places adn a list of PlacesId to filter it return the marking of
    /// all places parsed as a string and filtered based on pidsFilter.
    let markingsAsStringList (places: Places) pidsFilter =
        [] 
        |> Map.foldBack (fun pid placeData acc ->
            match pidsFilter |> List.contains pid with 
            | false -> acc
            | true -> (pid, markingAsString placeData) :: acc    
        ) places

    let removeTokens removeQty pid (places: Places) =
        // FIXME: the pick is random in a list of multisets, not implemented 
        // value nor colour
        match places |> Map.tryFind pid with
        | None -> Error <| InexistenPid (pid, places)
        | Some placeData -> 
            match placeData.marking with 
            | [] -> Error <| InsufficientTokens (pid, places) 
            | [{ qty = actQty }] when actQty = removeQty -> Ok <| []
            | [ token ] -> Ok <| [{token with qty = token.qty - removeQty}]
            | _token :: _rest as tokenList -> 
                    tokenList        
                    |> randomizeList         
                    |> fun (token :: rest) -> 
                        Ok <| {token with qty = token.qty - removeQty} :: rest

            >>= fun newMarking ->
                Ok <| places.
                        Remove(pid).
                        Add(pid, { placeData with marking = newMarking })
    
    let addTokens addQty pid (places: Places) =
        match places |> Map.tryFind pid with
        | None -> Error <| InexistenPid (pid, places)
        | Some placeData -> 
            match placeData.marking with 
            | [] -> 
                match placeData.colour |> ColorSet.randomVal with
                | Error _ -> Error <| UnexectedError "Can't initialize random"
                | Ok random ->
                    Ok [{ qty = 1; value = random; colour = placeData.colour }]
            | _token :: _rest as tokenList -> 
                    tokenList        
                    |> randomizeList         
                    |> fun (token :: rest) -> 
                        Ok <| {token with qty = token.qty + addQty} :: rest

            >>= fun newMarking ->
                Ok <| places.
                        Remove(pid).
                        Add(pid, { placeData with marking = newMarking })
    
    let modifyTokensForList modifyFunc qty pids (places: Places) =        
        pids 
        |> List.fold (fun acc pid -> acc >>= (modifyFunc qty pid)) (Ok places)