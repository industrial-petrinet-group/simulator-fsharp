namespace CPN.Simulator.Tests.Domain.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Domain
open CPN.Simulator.Tests.Domain

module BooleanTests =
    
    let preTest = Setup.declarations()

    [<Tests>]
    let tests =
        testList "Domain.ColorSets.BooleanTests." [
            testCase "Boolean color set can be created and it's value for falsy is false" <| fun () ->
                Ok <| Bool false =! ColorSet.deserialize (CS "bool") "false"
            
            //testCase "create and colorVal work as expected for Boolean" <| fun () ->
            //    let booleanCS = Boolean.create None
            //    let booleanWithedCS = Boolean.create (Some ("void", "whole"))

            //    Boolean.create (Some ("void", "void")) =! (Error <| CSErrors (InvalidInitialState "falsy and truthy must be different"))

            //    (booleanCS >>= Boolean.colorVal "true") =! (booleanWithedCS >>= Boolean.colorVal "whole")
            //    (booleanCS >>= Boolean.colorVal "true") =! Ok true

            //    (booleanCS >>= Boolean.colorVal "yes") =! (booleanWithedCS >>= Boolean.colorVal "yes")
            //    (booleanCS >>= Boolean.colorVal "yes") =! (Error <| CSErrors (InvalidValue "yes"))


            //testCase "Functions init and legal work as expected for Boolean" <| fun () ->
            //    let booleanCS = Boolean.create None
            //    let booleanWithedCS = Boolean.create (Some ("void", "whole"))

            //    Boolean.init =! false
                
            //    (booleanCS >>= switch (Boolean.isLegal false)) =! Ok true
            //    (booleanWithedCS >>= switch (Boolean.isLegal true)) =! Ok true
                
            //testCase "Small color set functions work as expected for Boolean" <| fun () ->
            //    let booleanCS = Boolean.create None
            //    let booleanWithedCS = Boolean.create (Some ("nulo", "todo"))

            //    (booleanCS >>= Boolean.all) =! (booleanWithedCS >>= Boolean.all)
            //    (booleanCS >>= Boolean.all) =! Ok [ false; true ]

            //    (booleanCS >>= Boolean.size) =! (booleanWithedCS >>= Boolean.size)
            //    (booleanCS >>= Boolean.size) =! Ok 2

            //    (booleanCS >>= Boolean.ordinal false) =! (booleanWithedCS >>= Boolean.ordinal false)
            //    (booleanCS >>= Boolean.ordinal false) =! Ok 0
                
            //    (booleanCS >>= Boolean.color 1) =! (booleanWithedCS >>= Boolean.color 1)
            //    (booleanCS >>= Boolean.color 1) =! Ok true

            //    (booleanCS >>= Boolean.color 2) =! (booleanWithedCS >>= Boolean.color 2)
            //    (booleanCS >>= Boolean.color 2) =! (Error <| CSErrors (OutOfRangeIndex 2))

            //    let csRandom = (booleanCS >>= Boolean.random) 
            //    (csRandom = Ok true || csRandom = Ok false) =! true
                
            //    let withedCSRandom = (booleanWithedCS >>= Boolean.random)
            //    (withedCSRandom = Ok true || withedCSRandom = Ok false) =! true

            //    // Check that the function produces actually random variables.
                
            //testCase "makeString work as expected for Boolean" <| fun () ->
            //    let booleanCS = Boolean.create None
            //    let booleanWithedCS = Boolean.create (Some ("null", "some"))

            //    (booleanCS >>= Boolean.makeString true) =! Ok "true"

            //    (booleanWithedCS >>= Boolean.makeString false) =! Ok "null"
        ]