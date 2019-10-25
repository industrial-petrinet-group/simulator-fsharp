module CPN.Simulator.Tests

open Expecto

[<EntryPoint>]
let main argv =
    Tests.runTestsInAssembly defaultConfig argv |> ignore

    System.Console.ReadKey() |> ignore

    0
