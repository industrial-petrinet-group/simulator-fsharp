namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing an element of a Multi Set
type Token = 
    { qty: int
      value: string }

/// Type representing a Multi Set
[<StructuredFormatDisplay("MultiSet = {Show}")>]
type MultiSet = 
    { set: Set<Token>
      color: ColorSet }

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

        /// Given a Token list ir reduce it
        let reduceTokenList redundantTokenList = 
            redundantTokenList
            |> List.groupBy (fun {value = value} -> value)
            |> List.map (fun (_, equalTokenList) -> 
                equalTokenList
                |> List.fold (fun acc {qty = actQty} -> 
                    { acc with qty = acc.qty + actQty }
                ) { (equalTokenList |> List.head) with qty = 0 })
    
        // Given a Token list it reduce it and returns a set of them
        let setOfTokenList tokenList =
            tokenList
            |> reduceTokenList
            |> List.fold (fun acc token -> acc |> Set.add token) emptyTS
    

    /// Given a color it creates and empty MultiSet
    let empty color = { set = Set.empty<Token> ; color = color }

    /// Given a MultiSet check if it's empty
    let isEmpty { set = multiSet } = 
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
                    >>= fun colorVal -> 
                        match qty |> System.Int32.TryParse with
                        | true, intVal -> Ok (intVal, colorVal)
                        | false, _ -> Error <| MSErrors (BadFormattedInputString inputString)
                    >>= fun (intQty, _) -> 
                        Ok { qty = intQty; value = value }
                    |> function
                        | Error err -> Error err
                        | Ok token -> Ok (token :: acc)
            ) (Ok [])
            >>= switch reduceTokenList
            >>= switch setOfTokenList
            >>= fun tokenSet -> Ok { (empty color) with set = tokenSet }
        | _ -> Error <| MSErrors (BadFormattedInputString inputString)

    /// Given a MultiSet it returns it's elements parsed as a single string.
    let asString { set = placeMarking } = 
        "" |> Set.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking

    let removeTokens removeQty {set = multiSet; color = color} =
        match multiSet with
        | Empty -> Error <| MSErrors InsufficientTokens 
        | Unique { qty = actQty } when actQty = removeQty -> Ok <| emptyTS
        | Unique token -> Ok <| emptyTS.Add({token with qty = token.qty - removeQty})
        | Set (token, restOfSet) -> Ok <| restOfSet.Add({token with qty = token.qty - removeQty})
        >>= fun newSet ->
            Ok <| {set = newSet; color = color}

    let addTokens addedQty {set = multiSet; color = color} =  
        match multiSet with
        | Empty ->
            match color |> ColorSet.randomVal with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add({ qty = addedQty; value = random })
        | Unique token -> Ok <| emptyTS.Add({token with qty = token.qty + addedQty})
        | Set (token, restOfSet) -> Ok <| restOfSet.Add({token with qty = token.qty + addedQty})
        >>= fun newSet ->
            Ok <| {set = newSet; color = color}

type MultiSet with
    /// Reimplements the way of showing the CPN
    member this.Show =
        let { color = color } = this

        this
        |> MultiSet.asString 
        |> fun elements -> 
            sprintf "%s (%A)" elements color