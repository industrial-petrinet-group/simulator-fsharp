namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator
open CPN.Simulator.Domain

module RuntimeTests =

    open System.Collections.Generic
    

    // let simplifyTriggered =
    //     Seq.map (fun (keyVal: KeyValuePair<Transition, Place list>) -> 
    //         keyVal.Key.name, 
    //         keyVal.Value |> List.map (fun p -> p.name))
    //     >> Seq.toList
    
    // [<Tests>]
    // let tests = 
    //     testList "RuntimeTests" [
    //         testCase "test the marking of the simple net to be P1 with 1 unit value" <| fun () ->
    //             SampleNets.simpleNet
    //             |> Runtime.netMarking 
    //             |> simplifyNetMarking
    //             =! ["P1", "1`()"]

    //             SampleNets.notSoSimpleNet
    //             |> Runtime.netMarking 
    //             |> simplifyNetMarking
    //             =! [("P1", "3`()"); ("P2", "1`()")]
            
    //         testCase "test the triggered transitions" <| fun () ->
    //             SampleNets.simpleNet
    //             |> Runtime.trigger
    //             |> simplifyTriggered
    //             =! ["T1", ["P1"]]

    //             SampleNets.notSoSimpleNet
    //             |> Runtime.trigger
    //             |> simplifyTriggered
    //             =! ["T1", ["P1"; "P2"]]

    //         testCase "test the steps involved in the simple net" <| fun () ->
    //             let modified, firstStepNet = SampleNets.simpleNet |> Runtime.step

    //             modified =! true 

    //             firstStepNet 
    //             |> Runtime.trigger 
    //             |> simplifyTriggered
    //             =! []

    //             firstStepNet
    //             |> Runtime.netMarking
    //             |> simplifyNetMarking
    //             =! ["P2", "1`()"]
            
    //         testCase "test the steps involved in the not so simple net" <| fun () ->
                
    //             printfn "%A" (SampleNets.notSoSimpleNet |> Runtime.allSteps)

    //             true =! true
    //     ]