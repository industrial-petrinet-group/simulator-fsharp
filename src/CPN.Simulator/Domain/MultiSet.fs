namespace CPN.Simulator.Domain

/// Type representing an element of a Multi Set
//type MSElement<'T> = 
//    { qty: int
//      value: 'T }

//type MultiSet<'T when 'T: comparison> = MS of Set<MSElement<'T>>

/// Type representing a Multi Set
type MultiSet = 
    { qty: int
      value: string
      colour: ColorSet }

/// Module implementing MultiSet's operations
module MultiSet =
    /// Given a MultiSet string it evaluates the expressions, reduce the equal
    /// values and generate a MultiSet.
    //let ofString msString =
        
    /// Given a MultiSet list return it's elements parsed as a single string.
    let listAsString placeMarking = 
        "" |> List.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking