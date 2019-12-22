#nowarn "025"
namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator
open CPN.Simulator.Domain

module RuntimeTests =
    
    let emptyExpr = E ""

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
                =! Transitions (Map.empty.Add(T 1, { name= "T1"; guard = emptyExpr
                                                     inputs = [(P 1, emptyExpr)]
                                                     outputs = [(P 2, emptyExpr)] }))

                SampleNets.notSoSimpleNet
                |> CPN.enabled
                =! Transitions (Map.empty.Add(T 1, { name= "T1"; guard = emptyExpr
                                                     inputs = [(P 1, emptyExpr); (P 2, emptyExpr)]
                                                     outputs = [(P 2, emptyExpr); (P 3, emptyExpr)] }))

            testCase "Test the steps involved in the simple net" <| fun () ->
                let (Ok (modified, firstStepNet)) = 
                    SampleNets.simpleNet |> Runtime.step

                modified =! true 

                firstStepNet 
                |> CPN.enabled
                =! Transitions (Map.empty)

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