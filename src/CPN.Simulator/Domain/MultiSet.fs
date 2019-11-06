namespace CPN.Simulator.Domain

/// Type representing a Multi Set
type MultiSet = 
    { qty: int
      value: string
      colour: ColorSet }

/// Module implementing MultiSet's operations
module MultiSet =
    /// Given a MultiSet list return it's elements parsed as a single string.
    let listAsString placeMarking = 
        "" |> List.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking