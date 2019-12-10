namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing a Place Id
type PlaceId = P of int

type MSCrate =
    abstract member Apply : MSCrateEvaluator<'ret> -> 'ret
and MSCrateEvaluator<'ret> =
    abstract member Eval<'a when 'a : comparison> : MultiSet<'a> -> 'ret  

module MSCrate =
    
    let make (ms : MultiSet<'a>) =
        { new MSCrate with
            member __.Apply e = e.Eval ms }
            
    let extract (msCrate : MSCrate) =
        msCrate.Apply 
            { new MSCrateEvaluator<obj> with
                 member __.Eval ms = box ms } |> unbox

    let isEmpty (msCrate : MSCrate) = //MultiSet.asString
        msCrate.Apply 
            { new MSCrateEvaluator<_> with
                member __.Eval ms = ms |> MultiSet.isEmpty }
    
    let addTokens addQty (msCrate : MSCrate) =
        msCrate.Apply 
            { new MSCrateEvaluator<_> with
                member __.Eval ms = 
                    ms |> MultiSet.addTokens addQty
                    >>= fun modifiedMS -> Ok <| make modifiedMS }

    let removeTokens removeQty (msCrate : MSCrate) =
        msCrate.Apply 
            { new MSCrateEvaluator<_> with
                member __.Eval ms = 
                    ms |> MultiSet.removeTokens removeQty
                    >>= fun modifiedMS -> Ok <| make modifiedMS }

    let asString (msCrate : MSCrate) = //MultiSet.asString
        msCrate.Apply 
            { new MSCrateEvaluator<_> with
                member __.Eval ms = ms |> MultiSet.asString }

/// Type representing a Place Data
type PlaceData = 
    { name: string
      marking: MSCrate }

/// Type representing a collection of places
type Places = 
    | Places of Map<PlaceId, PlaceData>

/// Module implementing Place's operations.
module Place =
    /// Module for for private implementation details
    [<AutoOpen>]
    module private Implementation =

        /// Given a PlaceData it return it's marking parsed as a string.
        let markingAsString placeData = placeData.marking |> MSCrate.asString 
    
        /// Given Places and a list of PlacesId to filter it returns a list of the 
        /// marking of all places parsed as a string and filtered based on the 
        /// filtering pids.
        let markingsAsStringList pidsFilter (Places places) =
            [] 
            |> Map.foldBack (fun pid placeData acc ->
                match pidsFilter |> List.contains pid with 
                | false -> acc
                | true -> (pid, markingAsString placeData) :: acc    
            ) places


    /// Given Places it returns new Places with it's multiset values reduced
    let reduceMarking places =
        // TODO
        places

    /// Given Places and a pid it return if it has at least one token
    let hasTokens pid (Places places) =
        match places |> Map.tryFind pid with
        | None -> false
        | Some placeData -> not (placeData.marking |> MSCrate.isEmpty)  
    
    /// Given the places it returns the string list marking for every PlaceID
    let placesMarkingAsStringList (Places places) =
        places
        |> Map.toList
        |> List.map fst
        |> List.distinct
        |> markingsAsStringList <| Places places

    let removeTokens removeQty pid (Places places) =
        // FIXME: the pick is random in a list of multisets, not implemented 
        // value nor color
        match places |> Map.tryFind pid with
        | None -> Error <| PErrors (InexistenPid (pid, Places places))
        | Some placeData -> 
            placeData.marking
            |> MSCrate.removeTokens removeQty
            |> function
                | Error _ -> Error <| PErrors (InsufficientTokensOn (pid, Places places))
                | Ok newMarking ->
                    Ok (Places <| places.
                                    Remove(pid).
                                    Add(pid, { placeData with marking = newMarking }))
    
    let addTokens addQty pid (Places places) =
        match places |> Map.tryFind pid with
        | None -> Error <| PErrors (InexistenPid (pid, Places places))
        | Some placeData -> 
            placeData.marking
            |> MSCrate.addTokens addQty
            >>= fun newMarking ->
                Ok (Places <| places.
                                Remove(pid).
                                Add(pid, { placeData with marking = newMarking}))
    
    let modifyTokensForList modifyFunc qty pids places =        
        pids 
        |> List.fold (fun acc pid -> acc >>= (modifyFunc qty pid)) (Ok places)