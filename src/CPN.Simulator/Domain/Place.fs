namespace CPN.Simulator.Domain

/// Type representing a Place Id
type PlaceId = P of int

/// Type representing a Place Data
type PlaceData = 
    { name: string
      colour: ColorSet 
      marking: MultiSet list }

/// Type representing a collection of places
type Places = Map<PlaceId, PlaceData>

/// Module implementing Place's operations.
module Place =
    /// Given Places and a pid it return if it has at least one token
    let hasTokens places pid =
        match places |> Map.tryFind pid with
        | None -> false
        | Some placeData -> placeData.marking <> [] // FIXME: only works for a unique CS    

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