module UnitTests

open Expecto
open Swensen.Unquote
open CPN.Simulator.ColorSets

[<Tests>]
let tests =
    testList "ColorSets/UnitTests" [
        testCase "Unit color set can be created and it's value is ()" <| fun () ->
            Unit.create None |> Unit.colorVal "()" =! Ok ()
    ]