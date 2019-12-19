namespace CPN.Simulator.Domain

open CPN.Simulator.Domain.ColorSets

/// Type representing a Color Set ID
type ColorSetId = CS of string

/// Type representing a Color Set Declaration
type Declarations = Declarations of Map<ColorSetId, IColorSet>

module Declarations =

    let defaults =
        let (Ok unitCS) = UnitCS.create None
        let (Ok unitCS') = UnitCS.create <| Some "none"
        let (Ok boolCS) = BoolCS.create None
        let (Ok boolCS') = BoolCS.create <| Some ("none", "whole")

        Map.empty
        |> Map.add (CS "void") (VoidCS :> IColorSet)
        |> Map.add (CS "unit") (unitCS :> IColorSet)
        |> Map.add (CS "unit'") (unitCS' :> IColorSet)
        |> Map.add (CS "bool") (boolCS :> IColorSet)
        |> Map.add (CS "bool'") (boolCS' :> IColorSet)
        |> Declarations

    let colorSet (Declarations declarations) csid = 
        declarations 
        |> Map.tryFind csid
        |> function
            | Some colorSet -> Ok colorSet
            | None -> Error <| DErrors UndeclaredColorSet