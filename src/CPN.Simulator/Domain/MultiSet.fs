namespace CPN.Simulator.Domain

/// Type representing a Multi Set by a qty; a string value and it's color.
type MultiSet = 
    { qty: int
      value: string
      colour: ColorSet }

/// Module implementing Multi Set operations
module MultiSet =
    /// parse a multi set color list to string
    let makeListString placeMarking = 
        "" |> List.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking