namespace CPN.Simulator.Tests

open Expecto

module Run =
    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv |> ignore

        System.Console.ReadKey() |> ignore

        0
