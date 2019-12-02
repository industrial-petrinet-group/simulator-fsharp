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
    
    member xMS.colorMatch yMS = 
        let (MS x, MS y) = xMS, yMS
        x.color = y.color || x.color = ColorSet.empty || y.color = ColorSet.empty

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

            match yObj with
            | :? MultiSet as yMS when xMS = yMS -> 0
            | :? MultiSet as yMS when xMS.colorMatch yMS -> 
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
                //match random with
                //| false -> 
                //    multiset 
                //    |> Map.pick (fun key qty -> Some (key, qty))
                //    |> fun ((key, _) as token) ->
                //        Set (token, multiset |> Map.remove key)
                //| true -> 
                //    multiset
                //    |> Map.toList
                //    |> randomizeList         
                //    |> fun (token :: rest) ->
                //        RandomSet (token, rest |> Map.ofList) 

        /// Empty Token Set
        let emptyTS = Map.empty<string, int>

        /// Given two MultiSets it returns if they colors match
        let colorMatch (xMS: MultiSet) yMS = xMS.colorMatch yMS

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
    
    /// Given two MultiSets it returns the first with the elements of the
    /// second added.
    let add (MS x as xMS) (MS y as yMS) =
        match (colorMatch xMS yMS) with
        | false -> Error <| MSErrors (UnmatchedColors [x.color.ToString(); y.color.ToString()])
        | true -> 
            let { values = xVals }, { values = yVals } = x, y
            
            yVals
            |> Map.fold (fun acc key qty ->
                match acc |> Map.tryFind(key) with
                | None -> acc |> Map.add key qty
                | Some actQty -> 
                    acc |> Map.remove(key) |> Map.add key (actQty + qty)) xVals
            |> fun addedValues -> 
                Ok <| MS { values = addedValues; color = x.color }

    /// Given two MultiSets it returns the first without the elements of the 
    /// second; it only works if the second one is less than or equal.
    let remove (MS x as xMS) (MS y as yMS) =
        match (colorMatch xMS yMS) with
        | false -> Error <| MSErrors (UnmatchedColors [x.color.ToString(); y.color.ToString()])
        | true -> 
            try 
                match (yMS <= xMS) with
                | false -> Error <| MSErrors SubstractorShouldBeLessOrEqual 
                | true -> 
                    let { values = xVals }, { values = yVals } = x, y
                    
                    yVals
                    |> Map.fold (fun acc key qty ->
                        acc 
                        |> Map.find key 
                        |> fun actQty ->
                            match (actQty - qty) with
                            | 0 -> acc |> Map.remove key
                            | newQty -> acc |> Map.remove key |> Map.add key newQty) xVals
                    |> fun removedValues -> 
                        Ok <| MS { values = removedValues; color = x.color }
            with 
            | :? System.ArgumentException -> Error <| MSErrors SubstractorShouldBeLessOrEqual 
    
    /// Given a scalar value and a multiset it multiply every qty asociated to
    /// the values of the multiset by the scalar.
    let scalarMultiply k (MS ms) =
        ms.values 
        |> Map.fold (fun acc key qty -> acc |> Map.add key (k * qty)) emptyTS
        |> fun multipliedValues -> MS { ms with values = multipliedValues }
    
    /// Given a multiset it returns it's size
    let size (MS { values = multiset }) =
        multiset |> Map.fold (fun acc _ qty -> acc + qty) 0
    
    /// Given a multiset it returs a random value
    // FIXME: better managing of randomness counting the number of elements per 
    // value, use size, a random up to it and fold the map counting the number
    let random (MS { values = multiset }) =
        let msList = multiset |> Map.toList

        msList |> List.item (random.Next(msList.Length)) |> fst
    
    /// Given a color value and a multiset it returns the number of ocurrences 
    /// of it in the multiset.
    let colorOcurrences colorVal (MS { values = multiset }) =
        multiset |> Map.tryFind colorVal |> function None -> 0 | Some n -> n
    
    /// Given a predicate and a multiset it returns only the values that satisfy
    /// the predicate
    let filter predicate (MS ({ values = multiset } as msData)) = 
        { msData with values = multiset |> Map.filter predicate }
    
    let map mapping (MS { values = multiset }) =
        multiset 
        |> Map.fold (fun acc key qty -> 
            acc |> Map.add (mapping key) qty) emptyTS
    
    // TODO: Check in CPNTools if this is the case!
    let fold folder (MS { values = multiset }) =
        multiset 
        |> Map.fold (fun acc key qty -> 
            acc + (folder key) * qty) 1
        
    /// Given a removeQty and a MultiSet it returns a MultiSet with the qty 
    /// removed
    let removeTokens rmQty (MS { values = multiSet; color = color }) =
        match multiSet with
        | Empty -> Error <| MSErrors InsufficientTokens 
        | Unique (_, qty) when qty < rmQty -> Error <| MSErrors InsufficientTokens 
        | Unique (_, qty) when qty = rmQty -> Ok <| emptyTS
        | Unique (value, qty) -> Ok <| emptyTS.Add((value, qty - rmQty))
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty - rmQty)  // FIXME: It should check all of the above
        >>= fun newValues ->
            Ok <| MS { values = newValues; color = color }
    
    /// Given a addedQty and a MultiSet it returns a MultiSet with the qty 
    /// added
    let addTokens addQty (MS { values = multiSet; color = color }) =  
        match multiSet with
        | Empty ->
            match color |> ColorSet.randomVal with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add(random, addQty)
        | Unique (value, qty) -> Ok <| emptyTS.Add(value, qty + addQty)
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty + addQty)
        >>= fun newValues ->
            Ok <| MS { values = newValues; color = color }

type MultiSet with
    /// Static operator implementation of MultiSet.add
    static member ( ++ ) (xMS, yMS) = MultiSet.add xMS yMS
    
    /// Static operator implementation of MultiSet.remove
    static member ( -- ) (xMS, yMS) = MultiSet.remove xMS yMS
    
    /// Static operator ( ** ) implementation of MultiSet.scalarMultiply
    static member Pow (xMS, k) = MultiSet.scalarMultiply k xMS

    ///// Static operator implementation of MultiSet.scalar
    //static member ( ** ) (xMS, k) = MultiSet.scalar k xMS
  
    /// Reimplements the way of showing the MultiSet
    member this.Show =
        let (MS { color = color }) = this

        this
        |> MultiSet.asString 
        |> fun elements -> 
            sprintf "%s (%A)" elements color