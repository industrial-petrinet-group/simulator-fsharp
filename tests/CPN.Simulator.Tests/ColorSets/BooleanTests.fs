module BooleanTests

open Expecto
open Swensen.Unquote
open CPN.Simulator.ColorSets

[<Tests>]
let tests =
    testList "ColorSets/BooleanTests" [
        testCase "Boolean color set can be created and it's value for falsy is false" <| fun () ->
            let boolCS = Boolean.create None

            boolCS 
            |> Boolean.colorVal "false"  
            =! (Ok false)
    ]