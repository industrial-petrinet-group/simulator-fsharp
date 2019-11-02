namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator

module RuntimeTests =

    [<Tests>]
    let tests = 
        testList "RuntimeTests" [
            testCase "test the marking of the simple net to be P1 with 1 unit value" <| fun () ->
                Runtime.simpleNet
                |> Runtime.netMarking 
                |> List.map (fun p -> p.name, Runtime.parseMultiSet p.marking)
                =! ["P1", "1`()"]
            
            testCase "test the triggered transitions" <| fun () ->
                // TODO: further test this and make mor complicated nets
                Runtime.simpleNet
                |> Runtime.trigger
                =! ([] |> Map.ofList)

        ]