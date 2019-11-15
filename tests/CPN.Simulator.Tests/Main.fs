namespace CPN.Simulator.Tests

open Expecto

module Runner =
    [<EntryPoint>]
    let main argv =
        Tests.runTestsInAssembly defaultConfig argv
