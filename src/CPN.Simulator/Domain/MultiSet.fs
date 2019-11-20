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
    

    /// Given a colour it creates and empty MultiSet
    let empty colour = { set = Set.empty<Token> ; colour = colour}

    /// Given a MultiSet check if it's empty
    let isEmpty { set = multiSet } = multiSet |> Set.isEmpty 
    
    /// Given a MultiSet string it evaluates the expressions, reduce the equal
    /// values and generate a MultiSet.
    //let ofString msString =  

    /// Given a MultiSet list return it's elements parsed as a single string.
    let listAsString { set = placeMarking } = 
        "" |> Set.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking

    let removeTokens removeQty {set = multiset; colour = colour} =
        match multiset with
        | Empty -> Error <| MSErrors InsufficientTokens 
        | Unique { qty = actQty } when actQty = removeQty -> Ok <| emptyTS
        | Unique token -> Ok <| emptyTS.Add({token with qty = token.qty - removeQty})
        | Set (token, restOfSet) -> Ok <| restOfSet.Add({token with qty = token.qty - removeQty})
        >>= fun newSet ->
            Ok <| {set = newSet; colour = colour}

    let addTokens addedQty {set = multiset; colour = colour} =  
        match multiset with
        | Empty ->
            match colour |> ColorSet.randomVal with
            | Error err -> Error err
            | Ok random -> Ok <| emptyTS.Add({ qty = addedQty; value = random })
        | Unique token -> Ok <| emptyTS.Add({token with qty = token.qty + addedQty})
        | Set (token, restOfSet) -> Ok <| restOfSet.Add({token with qty = token.qty + addedQty})
        >>= fun newSet ->
            Ok <| {set = newSet; colour = colour}