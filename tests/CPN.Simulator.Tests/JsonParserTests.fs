#nowarn "025"
namespace CPN.Simulator.Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator

module JsonParserTests =

    [<Tests>]
    let tests = 
        testList "Base.JsonParserTests." [
            testCase "Test the creation of sample CPN" <| fun () ->
                let expected = "Ok CPN = { netMarking = [(P 1, \"1`()\"); (P 2, \"3`()\")]\n           transitions = [(T 1, { name = \"T1\"\n                                  guard = E \" \"\n                                  inputs = [(P 1, E \" \")]\n                                  outputs = [(P 2, E \" \")] })] }"
            
                expected =! sprintf "%A" (JsonParser.parse None)
        ]
