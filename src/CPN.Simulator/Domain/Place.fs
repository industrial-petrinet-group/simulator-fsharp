namespace CPN.Simulator.Domain

open System.Collections
open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets

/// Type representing a Place Id
type PlaceId = P of int

type IPlaceData =
    abstract member Name: string

/// Type representing a Place Data
type PlaceData<'T when 'T : comparison> = 
    { name: string
      color: IColorSet<'T> 
      marking: MultiSet<'T> }

    interface IPlaceData with member this.Name = this.name

/// Type representing a collection of places
type Places = 
    | Places of Map<PlaceId, IPlaceData>

module GenericMultiSet =
    let asString (placeData: IPlaceData) =      
        match placeData with
        | :? PlaceData<unit> as pd -> pd.marking |> MultiSet.asString
        | :? PlaceData<bool> as pd -> pd.marking |> MultiSet.asString

/// Module implementing Place's operations.
module Place =
    /// Module for for private implementation details
    [<AutoOpen>]
    module private Implementation =
        
        let inline specify (placeData: IPlaceData) : PlaceData<_> = 
            let internalType =
                (placeData :?> PlaceData<_>).color.MetaData.internalType
        
            let resultType =
                typeof<PlaceData<_>>.MakeGenericType internalType
        
            System.Convert.ChangeType(placeData, resultType) :?> PlaceData<_>
        

        /// Given a PlaceData it return it's marking parsed as a string.
        let markingAsString = GenericMultiSet.asString
    
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
        | Some placeData -> not ((specify placeData).marking |> MultiSet.isEmpty)  
    
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
            (specify placeData).marking 
            |> MultiSet.removeTokens removeQty
            |> function
                | Error _ -> Error <| PErrors (InsufficientTokensOn (pid, Places places))
                | Ok newMarking ->
                    Ok (Places <| places.
                                    Remove(pid).
                                    Add(pid, { (specify placeData) with marking = newMarking }))
    
    let addTokens addQty pid (Places places) =
        match places |> Map.tryFind pid with
        | None -> Error <| PErrors (InexistenPid (pid, Places places))
        | Some placeData -> 
            (specify placeData).marking 
            |> MultiSet.addTokens addQty
            >>= fun newMarking ->
                Ok (Places <| places.
                                Remove(pid).
                                Add(pid, { (specify placeData) with marking = newMarking }))
    
    let modifyTokensForList modifyFunc qty pids places =        
        pids 
        |> List.fold (fun acc pid -> acc >>= (modifyFunc qty pid)) (Ok places)