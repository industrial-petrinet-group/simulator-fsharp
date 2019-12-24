namespace CPN.Simulator.Tests.Domain.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets

module FloatCSTests =

    [<Tests>]
    let tests =
        testList "Domain.ColorSets.FloatCSTests." [
            testCase "Float color set can be creted and it's value for '3,1415' is 3,1415" <| fun () ->
                Ok <| Float 3.1415 =! ColorSet.deserialize (CS "float") "3.1415"
            
            //testCase "create and colorVal work as expected for Double" <| fun () ->
            //    let doubleCS = Double.create None
            //    let doubleWithedCS = Double.create (Some ("20.1", "2345.67"))
                
            //    Double.create (Some ("25.1", "22.2")) =! (Error <| CSErrors (InvalidInitialState "low must be less than or equal to high"))
            //    Double.create (Some ("false", "20")) =! (Error <| CSErrors (InvalidInitialState "low and high must be double (64bits) values"))

            //    (doubleCS >>= Double.colorVal "24154215.12354") 
            //    =! Ok 24154215.12354

            //    (doubleCS >>= Double.colorVal "false") =! (Error <| CSErrors (InvalidValue "false"))
            //    (doubleWithedCS >>= Double.colorVal "20.05") =! (Error <| CSErrors (OutOfRangeValue "20.05"))


            //testCase "Functions init and legal work as expected for Double" <| fun () ->
            //    let doubleCS = Double.create None
            //    let doubleWithedCS = Double.create (Some ("0.0", "9.10"))
                
            //    Double.init =! 0.0

            //    (doubleCS >>= switch (Double.isLegal 2.79)) =! Ok true
            //    (doubleWithedCS >>= switch (Double.isLegal 4.0)) =! Ok true
            //    (doubleWithedCS >>= switch (Double.isLegal 9.11)) =! Ok false
                
            //testCase "Small color set functions work as expected for Double" <| fun () ->
            //    let doubleCS = Double.create None
            //    let doubleWithedCS = Double.create (Some ("1", "10"))

            //    (doubleCS >>= Double.all) =! (Error <| CSErrors (NotUsable "all"))
            //    (doubleWithedCS >>= Double.all) =! (Error <| CSErrors (NotUsable "all"))

            //    (doubleCS >>= Double.size) =! (Error <| CSErrors (NotUsable "size"))
            //    (doubleWithedCS >>= Double.size) =! (Error <| CSErrors (NotUsable "size"))

            //    (doubleCS >>= Double.ordinal 5.4) =! (Error <| CSErrors (NotUsable "ordinal"))
            //    (doubleWithedCS >>= Double.ordinal 5.4) =! (Error <| CSErrors (NotUsable "ordinal"))
                
            //    (doubleCS >>= Double.color 1.0) =! (Error <| CSErrors (NotUsable "color"))
            //    (doubleWithedCS >>= Double.color 1.0) =! (Error <| CSErrors (NotUsable "color"))

            //    (doubleCS >>= Double.random) =! (Error <| CSErrors (NotUsable "random"))
                
            //testCase "makeString work as expected for Double" <| fun () ->
            //    let doubleCS = Double.create None
            //    let doubleWithedCS = Double.create (Some ("1", "5"))

            //    (doubleCS >>= Double.makeString "12.5") =! Ok "12.5"
            //    (doubleCS >>= Double.makeString "null") =! (Error <| CSErrors (InvalidValue "null"))

            //    (doubleWithedCS >>= Double.makeString "12") =! (Error <| CSErrors (OutOfRangeValue "12"))
            //    (doubleWithedCS >>= Double.makeString "4") =! Ok "4"
            //    (doubleWithedCS >>= Double.makeString "NotANumber") =! (Error <| CSErrors (InvalidValue "NotANumber"))

        ]