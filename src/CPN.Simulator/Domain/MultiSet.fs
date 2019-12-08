namespace CPN.Simulator.Domain

open CPN.Simulator.Domain
open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets

/// Type representing the data of a Multiset
type MultiSetData<'T when 'T : comparison> =
    { values: Map<'T, int>
      color: IColorSet<'T> }

/// Type representing a Multi Set
[<StructuredFormatDisplay("MultiSet = {Show}")>]
[<CustomEquality; CustomComparison>]
type MultiSet<'T when 'T : comparison> = 
    | MS of MultiSetData<'T>
    
    member xMS.SameColor yMS = 
        let (MS x, MS y) = xMS, yMS in x.color.MetaData = y.color.MetaData

    member xMS.OneIsEmpty yMS =
        let (MS x, MS y) = xMS, yMS
        x.color.MetaData = ColorSet.empty.MetaData || 
        y.color.MetaData = ColorSet.empty.MetaData

    override xMS.Equals(yObj) =
        match yObj with
        | :? MultiSet<'T> as yMS -> 
            let (MS x, MS y) = xMS, yMS
            (x.color = y.color) && (x.values = y.values)
        | _ -> false

    override xMS.GetHashCode() = let (MS x) = xMS in hash (x.color, x.values)

    interface System.IComparable with      
        member xMS.CompareTo yObj =
            let disjointCompare = compareMap "cannot compare disjoint multisets"

            // CHeck that colormatch accept the Empty set but it cannot compare
            // values for diferent maps; I need to treat Empty as an special Case
            match yObj with
            | :? MultiSet<'T> as yMS when xMS = yMS -> 0
            | :? MultiSet<'T> as yMS when xMS.OneIsEmpty yMS ->
                let (MS x, MS y) = xMS, yMS
                let bothEmpty = x.color.MetaData = ColorSet.empty.MetaData,
                                y.color.MetaData = ColorSet.empty.MetaData    
                match bothEmpty with
                | true, true -> 0
                | true, false -> -1
                | false, true -> 1
            | :? MultiSet<'T> as yMS when xMS.SameColor yMS -> 
                let (MS x, MS y) = xMS, yMS
                match compare (x.values |> Map.count) (y.values |> Map.count) with
                | 1 -> disjointCompare 1 y.values x.values
                | compared -> disjointCompare compared x.values y.values
            | :? MultiSet<_> -> invalidArg "yObj" "cannot compare multisets of different colors"
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
        let emptyTS<'T when 'T: comparison> = Map.empty<'T, int>

        /// Given two MultiSets it returns if they colors match
        let sameColorOrEmpty (xMS: MultiSet<_>) yMS = 
            xMS.SameColor yMS || xMS.OneIsEmpty yMS

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
    
    let unbox func = fun (MS msData as ms) -> func ms

    /// It creates and empty MultiSet
    let empty = MS { values = emptyTS<unit> ; color = ColorSet.empty }

    /// Given a color it creates and empty MultiSet color-bound
    let emptyWithColor color = 
        MS { values = emptyTS ; color = color }

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
                    |> ColorSet.colorValue value
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
            | "" -> sprintf "%i`%A" qty value 
            | acc -> sprintf "%s++%i`%A" acc qty value 
        ) placeMarking

    /// Given two MultiSets it returns the first with the elements of the
    /// second added.
    let add (MS x as xMS) (MS y as yMS) =
        match (sameColorOrEmpty xMS yMS) with
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
        match (sameColorOrEmpty xMS yMS) with
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
    let random (MS { values = multiset; color = color} as ms) =
        multiset 
        |> Map.fold (fun (finish, acc, result) value qty ->
            match finish, acc < qty with
            | true, _ -> finish, acc, result
            | false, true -> true, acc, value
            | false, false -> false, acc - qty, result
        ) (false, random.Next(ms |> size), color.Init())
        |> fun (_, _, result) -> result
    
    /// Given a color value and a multiset it returns the number of ocurrences 
    /// of it in the multiset.
    let colorOcurrences colorVal (MS { values = multiset }) =
        multiset |> Map.tryFind colorVal |> function None -> 0 | Some n -> n
    
    /// Given a predicate and a multiset it returns only the values that satisfy
    /// the predicate
    let filter predicate (MS ({ values = multiset } as msData)) = 
        MS { msData with values = multiset |> Map.filter predicate }
    
    /// Given a mapping function it produces a new one mapping every element
    // FIXME: Cant be a mapping between colors and it should be probably i need 
    // to rethink if being an string Map is the way to go or
    // take a generic approach and define multisets of every color.
    let mapColors mapping (MS ({ values = multiset } as ms)) =
        multiset 
        |> Map.fold (fun acc key qty -> 
            acc |> Map.add (mapping key) qty) emptyTS
        |> fun mappedValues -> MS { ms with values = mappedValues }
    
    /// Given a mapping function that turns a value into a MultiSet and a Multiset
    /// it returns a new one created from boths.
    let mapMultiSets mapping (MS { values = multiset; color = color}) =
        multiset 
        |> Map.fold (fun accRes key qty -> 
            accRes 
            >>= fun acc -> add acc (scalarMultiply qty (mapping key))
        ) (Ok <| emptyWithColor color)
        
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
            match color |> ColorSet.randomValue with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add(random, addQty)
        | Unique (value, qty) -> Ok <| emptyTS.Add(value, qty + addQty)
        | Set ((value, qty), restOfVal) -> Ok <| restOfVal.Add(value, qty + addQty)
        >>= fun newValues ->
            Ok <| MS { values = newValues; color = color }

type MultiSet<'T when 'T: comparison> with
    /// Static operator implementation of MultiSet.add
    static member ( ++ ) (xMS, yMS) = MultiSet.add xMS yMS
    
    /// Static operator implementation of MultiSet.remove
    static member ( -- ) (xMS, yMS) = MultiSet.remove xMS yMS
    
    /// Static operator ( ** ) implementation of MultiSet.scalarMultiply
    static member Pow (xMS, k) = MultiSet.scalarMultiply k xMS

    /// Reimplements the way of showing the MultiSet
    member this.Show =
        let (MS { color = color }) = this

        this
        |> MultiSet.asString 
        |> fun elements -> 
            sprintf "%s (%A)" elements color