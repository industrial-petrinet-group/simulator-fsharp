namespace CPN.Simulator.Tests.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets
open CPN.Simulator.Domain.ColorSets.Common

module DoubleTests =

    [<Tests>]
    let tests =
        testList "ColorSets.DoubleTests." [
            testCase "Double color set can be creted and it's value for '3,1415' is 3,1415" <| fun () ->
                (Double.create None >>= Double.colorVal "3.1415") =! Ok 3.1415
            
            testCase "create and colorVal work as expected for Double" <| fun () ->
                let doubleCS = Double.create None
                let doubleWithedCS = Double.create (Some ("20.1", "2345.67"))
                
                Double.create (Some ("25.1", "22.2")) =! Error (InvalidInitialState "low must be less than or equal to high")
                Double.create (Some ("false", "20")) =! Error (InvalidInitialState "low and high must be double (64bits) values")

                (doubleCS >>= Double.colorVal "24154215.12354") 
                =! Ok 24154215.12354

                (doubleCS >>= Double.colorVal "false") =! Error (InvalidValue "false")
                (doubleWithedCS >>= Double.colorVal "20.05") =! Error (OutOfRangeValue "20.05")


            testCase "Functions init and legal work as expected for Double" <| fun () ->
                let doubleCS = Double.create None
                let doubleWithedCS = Double.create (Some ("0.0", "9.10"))
                
                Double.init =! 0.0

                (doubleCS >>= switch (Double.isLegal 2.79)) =! Ok true
                (doubleWithedCS >>= switch (Double.isLegal 4.0)) =! Ok true
                (doubleWithedCS >>= switch (Double.isLegal 9.11)) =! Ok false
                
            testCase "Small colour set functions work as expected for Double" <| fun () ->
                let doubleCS = Double.create None
                let doubleWithedCS = Double.create (Some ("1", "10"))

                (doubleCS >>= Double.all) =! Error (NotUsable "all")
                (doubleWithedCS >>= Double.all) =! Error (NotUsable "all")

                (doubleCS >>= Double.size) =! Error (NotUsable "size")
                (doubleWithedCS >>= Double.size) =! Error (NotUsable "size")

                (doubleCS >>= Double.ordinal 5.4) =! Error (NotUsable "ordinal")
                (doubleWithedCS >>= Double.ordinal 5.4) =! Error (NotUsable "ordinal")
                
                (doubleCS >>= Double.colour 1.0) =! Error (NotUsable "colour")
                (doubleWithedCS >>= Double.colour 1.0) =! Error (NotUsable "colour")

                (doubleCS >>= Double.random) =! Error (NotUsable "random")
                
            testCase "makeString work as expected for Double" <| fun () ->
                let doubleCS = Double.create None
                let doubleWithedCS = Double.create (Some ("1", "5"))

                (doubleCS >>= Double.makeString "12.5") =! Ok "12.5"
                (doubleCS >>= Double.makeString "null") =! Error (InvalidValue "null")

                (doubleWithedCS >>= Double.makeString "12") =! Error (OutOfRangeValue "12")
                (doubleWithedCS >>= Double.makeString "4") =! Ok "4"
                (doubleWithedCS >>= Double.makeString "NotANumber") =! Error (InvalidValue "NotANumber")

        ]