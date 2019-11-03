namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator

module RuntimeTests =

    open System.Collections.Generic
    
    let simplifyNetMarking =
        List.map (fun (p: Place) -> p.name, Runtime.parseMultiSet p.marking)    

    let simplifyTriggered =
        Seq.map (fun (keyVal: KeyValuePair<Transition, Place list>) -> 
            keyVal.Key.name, 
            keyVal.Value |> List.map (fun p -> p.name))
        >> Seq.toList
    
    [<Tests>]
    let tests = 
        testList "RuntimeTests" [
            testCase "test the marking of the simple net to be P1 with 1 unit value" <| fun () ->
                Runtime.simpleNet
                |> Runtime.netMarking 
                |> simplifyNetMarking
                =! ["P1", "1`()"]

                Runtime.notSoSimpleNet
                |> Runtime.netMarking 
                |> simplifyNetMarking
                =! [("P1", "3`()"); ("P2", "1`()")]
            
            testCase "test the triggered transitions" <| fun () ->
                Runtime.simpleNet
                |> Runtime.trigger
                |> simplifyTriggered
                =! ["T1", ["P1"]]

                Runtime.notSoSimpleNet
                |> Runtime.trigger
                |> simplifyTriggered
                =! ["T1", ["P1"; "P2"]]

            testCase "test the steps involved in the simple net" <| fun () ->
                let modified, firstStepNet = Runtime.simpleNet |> Runtime.step

                modified =! true 

                firstStepNet 
                |> Runtime.trigger 
                |> simplifyTriggered
                =! []

                firstStepNet
                |> Runtime.netMarking
                |> simplifyNetMarking
                =! ["P2", "1`()"]
        ]