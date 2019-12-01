namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing the data of a Multiset
type MultiSetData =
    { values: Map<string, int>
      color: ColorSet }

/// Type representing a Multi Set
[<StructuredFormatDisplay("MultiSet = {Show}")>]
[<CustomEquality; CustomComparison>]
type MultiSet = 
    | MS of MultiSetData
    
    override xMS.Equals(yObj) =
        match yObj with
        | :? MultiSet as yMS -> 
            let (MS x, MS y) = xMS, yMS
            (x.color = y.color) && (x.values = y.values)
        | _ -> false

    override xMS.GetHashCode() = let (MS x) = xMS in hash (x.color, x.values)

    interface System.IComparable with      
        member xMS.CompareTo yObj =
            let disjointCompare = compareMap "cannot compare disjoint multisets"
            let colorMatch xMS yMS = 
                let (MS x, MS y) = xMS, yMS
                x.color = y.color || x.color = ColorSet.empty || y.color = ColorSet.empty

            match yObj with
            | :? MultiSet as yMS when xMS = yMS -> 0
            | :? MultiSet as yMS when colorMatch xMS yMS -> 
                let (MS x, MS y) = xMS, yMS
                match compare (x.values |> Map.count) (y.values |> Map.count) with
                | 1 -> disjointCompare 1 y.values x.values
                | compared -> disjointCompare compared x.values y.values
            | :? MultiSet -> invalidArg "yObj" "cannot compare multisets of different colors"
            | _ -> invalidArg "yObj" "cannot compare values of different types"


/// Module implementing MultiSet's operations
module MultiSet =
    /// Module for for private implementation details
    [<AutoOpen>]
    module private Implementation =
        /// Active pattern for MultiSet pattern matching. 
        // TODO: Decide if random pick is the norm or a special case
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
    

    /// It creates and empty MultiSet
    let empty = MS { values = emptyTS ; color = ColorSet.empty }

    /// Given a color it creates and empty MultiSet color-bound
    let emptyWithColor color = MS { values = emptyTS ; color = color }

    /// Given a MultiSet check if it's empty
    // TODO: Check if it's needed to differentiate empty for emptyWithColor
    let isEmpty (MS { values = multiSet }) = 
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
                resAcc
                >>= fun acc ->
                    color 
                    |> ColorSet.colorVal value
                    >>= fun _colorVal -> 
                        match qty |> System.Int32.TryParse with
                        | true, intVal -> Ok (value, intVal)
                        | false, _ -> Error <| MSErrors (BadFormattedInputString inputString)
                    >>= fun token -> Ok (token :: acc)
            ) (Ok [])
            >>= switch mapOfTokenList
            >>= fun tokenMap -> Ok (MS { values = tokenMap; color = color })
        | _ -> Error <| MSErrors (BadFormattedInputString inputString)

    /// Given a MultiSet it returns it's elements parsed as a single string.
    let asString (MS { values = placeMarking }) = 
        "" |> Map.foldBack (fun value qty acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking
    
    /// Given a removeQty and a MultiSet it returns a MultiSet with the qty 
    /// removed
    let removeTokens rmQty (MS {values = multiSet; color = color}) =
        match multiSet with
        | Empty -> Error <| MSErrors InsufficientTokens 
        | Unique (_, qty) when qty < rmQty -> Error <| MSErrors InsufficientTokens 
        | Unique (_, qty) when qty = rmQty -> Ok <| emptyTS
        | Unique (value, qty) -> Ok <| emptyTS.Add((value, qty - rmQty))
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty - rmQty)  // FIXME: It should check all of the above
        >>= fun newValues ->
            Ok <| MS {values = newValues; color = color}
    
    /// Given a addedQty and a MultiSet it returns a MultiSet with the qty 
    /// added
    let addTokens addQty (MS {values = multiSet; color = color}) =  
        match multiSet with
        | Empty ->
            match color |> ColorSet.randomVal with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add(random, addQty)
        | Unique (value, qty) -> Ok <| emptyTS.Add(value, qty + addQty)
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty + addQty)
        >>= fun newValues ->
            Ok <| MS {values = newValues; color = color}

type MultiSet with
    /// Reimplements the way of showing the MultiSet
    member this.Show =
        let (MS { color = color }) = this

        this
        |> MultiSet.asString 
        |> fun elements -> 
            sprintf "%s (%A)" elements color