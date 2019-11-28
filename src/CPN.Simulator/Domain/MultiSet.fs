namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing a Multi Set
[<StructuredFormatDisplay("MultiSet = {Show}")>]
[<CustomEquality; CustomComparison>]
type MultiSet = 
    { values: Map<string, int>
      color: ColorSet }
    
    override x.Equals(yObj) =
        match yObj with
        | :? MultiSet as y -> (x.color = y.color) && (x.values = y.values)
        | _ -> false

    override x.GetHashCode() = hash (x.color, x.values)

    interface System.IComparable with      
        member x.CompareTo yObj =
            // FIXME: This is really complex, it should be refactored.
            // Compare 2 maps and return if one is less than the other or they
            // are disjoint
            let compareMap compareCount lessMap moreMap =
                let disjointExn () = invalidArg "yObj" "cannot compare disjoint multisets"
                let lessKeys, moreKeys = lessMap |> getKeys, moreMap |> getKeys

                match lessKeys = moreKeys, compareCount with
                | true, 0 ->
                    lessKeys |> List.fold (fun (mantained, lastComp) key ->
                        match mantained with
                        | false -> false, 0
                        | true -> 
                            let newComp = compare lessMap.[key] moreMap.[key]
                            lastComp = newComp || newComp = 0, 
                            if (newComp = 0) then lastComp else newComp
                    ) (true, (compare lessMap.[List.head lessKeys] moreMap.[List.head lessKeys]))
                    |> function
                        | true, compared -> compared
                        | false, _ -> disjointExn ()
                | false, _ ->
                    moreKeys 
                    |> List.fold (fun acc key ->
                        match acc with
                        | [] -> acc
                        | head :: tail -> if head = key then tail else head::tail 
                    ) lessKeys
                    |> function
                        | [] -> 
                            lessKeys 
                            |> List.forall (fun key -> lessMap.[key] <= moreMap.[key])
                            |> function
                                | true -> compareCount
                                | false -> disjointExn ()
                        | _ -> disjointExn ()

                | true, _ -> disjointExn ()
            
            

            match yObj with
            | :? MultiSet as y when x = y -> 0
            | :? MultiSet as y when x.color = y.color -> 
                match compare (x.values |> Map.count) (y.values |> Map.count) with
                | 1 -> compareMap 1 y.values x.values
                | compared -> compareMap compared x.values y.values
            | :? MultiSet -> invalidArg "yObj" "cannot compare multisets of different colors"
            | _ -> invalidArg "yObj" "cannot compare values of different types"


/// Module implementing MultiSet's operations
module MultiSet =
    /// Module for for private implementation details
    [<AutoOpen>]
    module private Implementation =
        /// Active pattern for MultiSet pattern matching
        let (|Empty|Unique|Set|) multiset =
            match multiset |> Map.count with
            | 0 -> Empty
            | 1 -> Unique (multiset |> Map.pick (fun value qty -> Some (value, qty)))
            | _ -> 
                multiset
                |> Map.toList
                |> randomizeList         
                |> fun (token :: rest) ->
                    Set (token, rest |> Map.ofList) 

        /// Empty Token Set
        let emptyTS = Map.empty<string, int>

        /// Given a Token list ir reduce it
        let reduceTokenList redundantTokenList = 
            redundantTokenList
            |> List.groupBy (fun (value, _qty) -> value)
            |> List.map (fun (value, equalTokenList) -> 
                value, 
                equalTokenList |> List.fold (fun acc (_, qty) -> acc + qty) 0)
    
        // Given a Token list it reduce it and returns a set of them
        let mapOfTokenList tokenList =
            tokenList
            |> reduceTokenList
            |> List.fold (fun acc (value, qty) -> acc |> Map.add value qty) emptyTS
    

    /// Given a color it creates and empty MultiSet
    let empty color = { values = emptyTS ; color = color }

    /// Given a MultiSet check if it's empty
    let isEmpty { values = multiSet } = 
        match multiSet with Empty -> true | _ -> false
    
    /// Given a MultiSet string it evaluates the expressions*, reduce the equal
    /// values and generate a MultiSet.
    // * TODO
    let ofString color inputString =
        match inputString with
        | Match @"(\d+)`(.+?)(?:\+{2}|$)" matches -> 
            matches 
            |> List.chunkBySize 2
            |> List.fold (fun resAcc [ qty; value ] ->
                match resAcc with
                | Error _ -> resAcc
                | Ok acc -> 
                    color 
                    |> ColorSet.colorVal value
                    >>= fun _colorVal -> 
                        match qty |> System.Int32.TryParse with
                        | true, intVal -> Ok (value, intVal)
                        | false, _ -> Error <| MSErrors (BadFormattedInputString inputString)
                    |> function
                        | Error err -> Error err
                        | Ok token -> Ok (token :: acc)
            ) (Ok [])
            >>= switch mapOfTokenList
            >>= fun tokenMap -> Ok { (empty color) with values = tokenMap }
        | _ -> Error <| MSErrors (BadFormattedInputString inputString)

    /// Given a MultiSet it returns it's elements parsed as a single string.
    let asString { values = placeMarking } = 
        "" |> Map.foldBack (fun value qty acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking
    
    /// Given a removeQty and a MultiSet it returns a MultiSet with the qty 
    /// removed
    let removeTokens rmQty {values = multiSet; color = color} =
        match multiSet with
        | Empty -> Error <| MSErrors InsufficientTokens 
        | Unique (_, qty) when qty < rmQty -> Error <| MSErrors InsufficientTokens 
        | Unique (_, qty) when qty = rmQty -> Ok <| emptyTS
        | Unique (value, qty) -> Ok <| emptyTS.Add((value, qty - rmQty))
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty - rmQty)  // FIXME: It should check all of the above
        >>= fun newValues ->
            Ok <| {values = newValues; color = color}
    
    /// Given a addedQty and a MultiSet it returns a MultiSet with the qty 
    /// added
    let addTokens addQty {values = multiSet; color = color} =  
        match multiSet with
        | Empty ->
            match color |> ColorSet.randomVal with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add(random, addQty)
        | Unique (value, qty) -> Ok <| emptyTS.Add(value, qty + addQty)
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty + addQty)
        >>= fun newValues ->
            Ok <| {values = newValues; color = color}

type MultiSet with
    /// Reimplements the way of showing the MultiSet
    member this.Show =
        let { color = color } = this

        this
        |> MultiSet.asString 
        |> fun elements -> 
            sprintf "%s (%A)" elements color