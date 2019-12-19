namespace CPN.Simulator.Domain

open CPN.Simulator.Domain.ColorSets

type Declarations = 
    | D of Map<ColorSetId, IColorSet>

module Declarations =

    let defaults =
        let (Ok unitCS) = UnitCS.create None
        let (Ok boolCS) = BoolCS.create None

        Map.empty
        |> Map.add (CS "void") (VoidCS :> IColorSet)
        |> Map.add (CS "unit") (unitCS :> IColorSet)
        |> Map.add (CS "bool") (boolCS :> IColorSet)
        |> D

    let colorSet (D declarations) csid = declarations |> Map.tryFind csid