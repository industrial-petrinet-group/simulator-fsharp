namespace CPN.Simulator.Domain

open CPN.Simulator.Domain.ColorSets

/// Type representing a Color Set ID
type ColorSetId = CS of string

/// Type representing a Color Set Declaration
type Declarations = Declarations of Map<ColorSetId, IColorSet>

module Declaration =
    
    let defaults =
        let (Ok unitCS) = UnitCS.create None 
        let (Ok boolCS) = BoolCS.create None

        Map.empty
        |> Map.add (CS "void") (VoidCS :> IColorSet)
        |> Map.add (CS "unit") (unitCS :> IColorSet)
        |> Map.add (CS "bool") (boolCS :> IColorSet)
        |> Declarations
    
    let mutable private state = defaults

    let actuals () = state

    let update news =
        let (Declarations defaultMap) = actuals ()

        news 
        |> List.fold (fun acc (csid, cs) -> acc |> Map.add csid cs ) defaultMap
        |> Declarations
        |> fun newDec -> (state <- newDec)
        
    let colorSet (Declarations declarations) csid = 
        declarations 
        |> Map.tryFind csid
        |> function
            | Some colorSet -> Ok colorSet
            | None -> Error <| DErrors UndeclaredColorSet