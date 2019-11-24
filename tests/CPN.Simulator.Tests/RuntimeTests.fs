#nowarn "025"
namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator
open CPN.Simulator.Domain

module RuntimeTests =
    
    [<Tests>]
    let tests = 
        testList "Base.RuntimeTests." [
            testCase "Test the marking of the simple net to be P1 with 1 unit value" <| fun () ->
                SampleNets.simpleNet
                |> CPN.netMarking
                =! [P 1, "1`()"]

                SampleNets.notSoSimpleNet
                |> CPN.netMarking 
                =! [(P 1, "3`()"); (P 2, "1`()")]
            
            testCase "Test the triggered transitions" <| fun () ->              
                SampleNets.simpleNet
                |> CPN.enabled
                =! Net (Map.empty.Add(T 1, {i = [(P 1, A 1)]; o = [(P 2, A 2)]}))

                SampleNets.notSoSimpleNet
                |> CPN.enabled
                =! Net (Map.empty.Add(T 1, { i = [(P 1, A 1); (P 2, A 2)]
                                             o = [(P 2, A 3); (P 3, A 4)]}))

            testCase "Test the steps involved in the simple net" <| fun () ->
                let (Ok (modified, firstStepNet)) = 
                    SampleNets.simpleNet |> Runtime.step

                modified =! true 

                firstStepNet 
                |> CPN.enabled
                =! Net (Map.empty)

                firstStepNet
                |> CPN.netMarking
                =! [P 2, "1`()"]
            
            testCase "Test the steps involved in the not so simple net" <| fun () ->
                let steps = SampleNets.notSoSimpleNet |> Runtime.allSteps

                steps |> Seq.length =! 5

                steps 
                |> Seq.last 
                |> CPN.netMarking 
                =! [(P 2, "1`()"); (P 4, "3`()"); (P 5, "3`()")]
                
            testCase "Test the steps involved in the randomly pathed net" <| fun () ->
                let nLastFromRPN = 
                    (fun _ -> SampleNets.randomlyPathedNet |> Runtime.allSteps)
                    |> List.init 10 
                    |> List.map (Seq.last >> CPN.netMarking)
                
                nLastFromRPN
                |> List.fold (fun (equals, prior) nLast ->
                    equals && prior = nLast, nLast
                ) (true, nLastFromRPN |> List.head)
                |> fst 
                =! false
        ]