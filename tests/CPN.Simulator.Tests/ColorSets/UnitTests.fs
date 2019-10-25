module UnitTests

open Expecto
open Swensen.Unquote
open CPN.Simulator.ColorSets

[<Tests>]
let tests =
    testList "ColorSets/UnitTests" [
        testCase "Unit color set can be created and it's value is ()" <| fun () ->
            let unitCS = Unit.create None

            unitCS 
            |> Unit.colorVal "()"  
            =! Ok ()
    ]