namespace CPN.Simulator.Tests.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.ColorSets
open CPN.Simulator.ColorSets.Common

module IntegerTests =

    [<Tests>]
    let tests =
        testList "ColorSets/IntegerTests" [
            testCase "Integer color set can be created and it's value for '135' is 135" <| fun () ->
                (Integer.create None >>= Integer.colorVal "135") =! Ok 135
            
            testCase "create and colorVal work as expected for Integer" <| fun () ->
                let integerCS = Integer.create None
                let integerWithedCS = Integer.create (Some ("20", "25"))
                
                Integer.create (Some ("25", "20")) =! Error (InvalidInitialState "low must be less than or equal to high")
                Integer.create (Some ("false", "20")) =! Error (InvalidInitialState "low and high must be integer (32bits) values")

                (integerCS >>= Integer.colorVal "23") =! (integerWithedCS >>= Integer.colorVal "23")
                (integerCS >>= Integer.colorVal "23") =! Ok 23

                (integerCS >>= Integer.colorVal "false") =! Error (InvalidValue "false")
                (integerWithedCS >>= Integer.colorVal "26") =! Error (OutOfRangeValue "26")


            testCase "Functions init and legal work as expected for Integer" <| fun () ->
                let integerCS = Integer.create None
                let integerWithedCS = Integer.create (Some ("0", "9"))
                
                Integer.init =! 0

                (integerCS >>= switch (Integer.isLegal 12)) =! Ok true
                (integerWithedCS >>= switch (Integer.isLegal 4)) =! Ok true
                (integerWithedCS >>= switch (Integer.isLegal 12)) =! Ok false
                
            testCase "Small colour set functions work as expected for Integer" <| fun () ->
                let integerCS = Integer.create None
                let integerWithedCS = Integer.create (Some ("1", "10"))

                (integerCS >>= Integer.all) =! Error (NotUsable "all")
                (integerWithedCS >>= Integer.all) =! Ok [1..10]

                (integerCS >>= Integer.size) =! Error (NotUsable "size")
                (integerWithedCS >>= Integer.size) =! Ok 10

                (integerCS >>= Integer.ordinal 5) =! Error (NotUsable "ordinal")
                (integerWithedCS >>= Integer.ordinal 5) =! Ok 4
                
                (integerCS >>= Integer.colour 1) =! Error (NotUsable "colour")
                (integerWithedCS >>= Integer.colour 1) =! Ok 2
                (integerWithedCS >>= Integer.colour 11) =! Error (OutOfRangeIndex 11)

                (integerCS >>= Integer.random) =! Error (NotUsable "random")
                
                let withedCSRandom = (integerWithedCS >>= Integer.random)
                let toTest = match withedCSRandom with Ok v -> v | Error _ -> -1
                
                (toTest > 0 && toTest < 11) =! true

                // Check that the function produces actually random variables.
                
            testCase "makeString work as expected for Integer" <| fun () ->
                let integerCS = Integer.create None
                let integerWithedCS = Integer.create (Some ("1", "5"))

                (integerCS >>= Integer.makeString "12") =! Ok "12"
                (integerCS >>= Integer.makeString "null") =! Error (InvalidValue "null")

                (integerWithedCS >>= Integer.makeString "12") =! Error (OutOfRangeValue "12")
                (integerWithedCS >>= Integer.makeString "4") =! Ok "4"
                (integerWithedCS >>= Integer.makeString "NaN") =! Error (InvalidValue "NaN")

        ]