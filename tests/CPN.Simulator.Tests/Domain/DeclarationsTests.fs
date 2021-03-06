﻿namespace CPN.Simulator.Tests.Domain

open Expecto
open Swensen.Unquote

open CPN.Simulator.Domain

module DeclarationsTests =

    [<Tests>]
    let tests = 
        testList "Domain.Declarations." [
            testCase "default declarations exist" <| fun () ->
               Declaration.actuals () |> ignore
               1 =! 1

        ]
