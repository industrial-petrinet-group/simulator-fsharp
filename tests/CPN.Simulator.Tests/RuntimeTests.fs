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
    
    [<Tests>]
    let tests = 
        testList "RuntimeTests" [
            testCase "test the marking of the simple net to be P1 with 1 unit value" <| fun () ->
                SampleNets.simpleNet
                |> CPN.netMarking
                =! [P 1, "1`()"]

                SampleNets.notSoSimpleNet
                |> CPN.netMarking 
                =! [(P 1, "3`()"); (P 2, "1`()")]
            
            testCase "test the triggered transitions" <| fun () ->              
                SampleNets.simpleNet
                |> CPN.toTrigger
                =! Map.empty.Add(T 1, {i = [(P 1, A 1)]; o = [(P 2, A 2)]})

                SampleNets.notSoSimpleNet
                |> CPN.toTrigger
                =! Map.empty.Add(T 1, { i = [(P 1, A 1); (P 2, A 2)]
                                        o = [(P 2, A 3); (P 3, A 4)]})

            testCase "test the steps involved in the simple net" <| fun () ->
                let (Ok (modified, firstStepNet)) = 
                    SampleNets.simpleNet |> Runtime.step

                modified =! true 

                firstStepNet 
                |> CPN.toTrigger
                =! Map.empty

                firstStepNet
                |> CPN.netMarking
                =! [P 2, "1`()"]
            
            testCase "test the steps involved in the not so simple net" <| fun () ->
                let steps = SampleNets.notSoSimpleNet |> Runtime.allSteps

                steps |> Seq.length =! 5

                steps 
                |> Seq.last 
                |> CPN.netMarking 
                =! [(P 2, "1`()"); (P 4, "3`()"); (P 5, "3`()")]                
        ]