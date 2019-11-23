namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing an element of a Multi Set
type Token = 
    { qty: int
      value: string }

/// Type representing a Multi Set
type MultiSet = 
    { set: Set<Token>
      colour: ColorSet }

/// Module implementing MultiSet's operations
module MultiSet =
    /// Module for for private implementation details
    [<AutoOpen>]
    module private Implementation =
        /// Active pattern for MultiSet pattern matching
        let (|Empty|Unique|Set|) multiset =
            match multiset |> Set.count with
            | 0 -> Empty
            | 1 -> Unique (multiset |> Seq.pick (fun x -> Some x))
            | _ -> 
                multiset
                |> Set.toList
                |> randomizeList         
                |> fun (token :: rest) ->
                    Set (token, rest |> Set.ofList) 

        /// Empty Token Set
        let emptyTS = Set.empty<Token>

        let reduceTokenList redundantTokenList = 
            redundantTokenList
            |> List.groupBy (fun {value = value} -> value)
            |> List.map (fun (_, equalTokenList) -> 
                equalTokenList
                |> List.fold (fun acc {qty = actQty} -> 
                    { acc with qty = acc.qty + actQty }
                ) { (equalTokenList |> List.head) with qty = 0 })

        let setOfTokenList tokenList =
            tokenList
            |> List.fold (fun acc token -> acc |> Set.add token) emptyTS
    

    /// Given a colour it creates and empty MultiSet
    let empty colour = { set = Set.empty<Token> ; colour = colour}

    /// Given a MultiSet check if it's empty
    let isEmpty { set = multiSet } = 
        match multiSet with Empty -> true | _ -> false
    
    /// Given a MultiSet string it evaluates the expressions*, reduce the equal
    /// values and generate a MultiSet.
    // * TODO
    let ofString colour inputString =
        match inputString with
        | Match @"(\d+)`(.+?)(?:\+{2}|$)" matches -> 
            matches 
            |> List.chunkBySize 2
            |> List.fold (fun resAcc [ qty; value ] ->
                match resAcc with
                | Error _ -> resAcc
                | Ok acc -> 
                    colour 
                    |> ColorSet.colorVal value
                    >>= fun colorVal -> 
                        match qty |> System.Int32.TryParse with
                        | true, intVal -> Ok (intVal, colorVal)
                        | false, _ -> Error <| MSErrors (BadFormattedInputString inputString)
                    >>= fun (intQty, colorVal) -> 
                        Ok { qty = intQty; value = colorVal }
                    |> function
                        | Error err -> Error err
                        | Ok token -> Ok (token :: acc)
            ) (Ok [])
            >>= switch reduceTokenList
            >>= switch setOfTokenList
            >>= fun tokenSet -> Ok { (empty colour) with set = tokenSet }
        | _ -> Error <| MSErrors (BadFormattedInputString inputString)


    /// Given a MultiSet list return it's elements parsed as a single string.
    let listAsString { set = placeMarking } = 
        "" |> Set.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking

    let removeTokens removeQty {set = multiSet; colour = colour} =
        match multiSet with
        | Empty -> Error <| MSErrors InsufficientTokens 
        | Unique { qty = actQty } when actQty = removeQty -> Ok <| emptyTS
        | Unique token -> Ok <| emptyTS.Add({token with qty = token.qty - removeQty})
        | Set (token, restOfSet) -> Ok <| restOfSet.Add({token with qty = token.qty - removeQty})
        >>= fun newSet ->
            Ok <| {set = newSet; colour = colour}

    let addTokens addedQty {set = multiSet; colour = colour} =  
        match multiSet with
        | Empty ->
            match colour |> ColorSet.randomVal with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add({ qty = addedQty; value = random })
        | Unique token -> Ok <| emptyTS.Add({token with qty = token.qty + addedQty})
        | Set (token, restOfSet) -> Ok <| restOfSet.Add({token with qty = token.qty + addedQty})
        >>= fun newSet ->
            Ok <| {set = newSet; colour = colour}