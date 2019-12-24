#nowarn "025"
namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote

open CPN.Simulator
open CPN.Simulator.Domain
open CPN.Simulator.Operators

module JsonParserTests =

    [<Tests>]
    let tests = 
        testList "Base.JsonParserTests." [
            testCase "Test the creation of the sample CPN" <| fun () ->
                let expected = [ P 1, "1`()"; P 2, "3`()" ]
            
                let marking = 
                    (JsonParser.parse None) 
                    >>= fun sample -> Ok <| CPN.netMarking sample

                Ok expected =! marking
            
            testCase "Test the steps of the sample CPN" <| fun () ->
                let expected = [ P 2, "4`()" ], 2
            
                let marking = 
                    (JsonParser.parse None) 
                    >>= fun sample -> 
                        Ok <|
                            ((Runtime.allSteps sample |> Seq.last),
                            (sample |> Runtime.allSteps |> Seq.length))
                    >>= fun (last, count) -> Ok <| (CPN.netMarking last, count)

                Ok expected =! marking
        ]
