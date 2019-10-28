module BigIntegerTests

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.ColorSets
open CPN.Simulator.ColorSets.Common

[<Tests>]
let tests =
    testList "ColorSets/BigIntegerTests" [
        testCase "BigInteger color set can be created and it's value for '1353' is 1353" <| fun () ->
            (BigInteger.create None >>= BigInteger.colorVal "1353") =! Ok 1353I
        
        testCase "create and colorVal work as expected for BigInteger" <| fun () ->
            let bigIntegerCS = BigInteger.create None
            let bigIntegerWithedCS = BigInteger.create (Some ("20", "23452123153426354875151654213215487511"))
            
            BigInteger.create (Some ("25", "22")) =! Error (InvalidInitialState "low must be less than or equal to high")
            BigInteger.create (Some ("false", "20")) =! Error (InvalidInitialState "low and high must be big integer values")

            (bigIntegerCS >>= BigInteger.colorVal "23452123153426354875151654213215487511") 
            =! Ok 23452123153426354875151654213215487511I

            (bigIntegerCS >>= BigInteger.colorVal "false") =! Error (InvalidValue "false")
            (bigIntegerWithedCS >>= BigInteger.colorVal "19") =! Error (OutOfRangeValue "19")


        testCase "Functions init and legal work as expected for BigInteger" <| fun () ->
            let bigIntegerCS = BigInteger.create None
            let bigIntegerWithedCS = BigInteger.create (Some ("0", "9"))
            
            BigInteger.init =! 0

            (bigIntegerCS >>= switch (BigInteger.isLegal 12I)) =! Ok true
            (bigIntegerWithedCS >>= switch (BigInteger.isLegal 4I)) =! Ok true
            (bigIntegerWithedCS >>= switch (BigInteger.isLegal 12I)) =! Ok false
            
        testCase "Small colour set functions work as expected for BigInteger" <| fun () ->
            let bigIntegerCS = BigInteger.create None
            let bigIntegerWithedCS = BigInteger.create (Some ("1", "10"))

            (bigIntegerCS >>= BigInteger.all) =! Error (NotUsable "all")
            (bigIntegerWithedCS >>= BigInteger.all) =! Ok [1I..10I]

            (bigIntegerCS >>= BigInteger.size) =! Error (NotUsable "size")
            (bigIntegerWithedCS >>= BigInteger.size) =! Ok 10I

            (bigIntegerCS >>= BigInteger.ordinal 5I) =! Error (NotUsable "ordinal")
            (bigIntegerWithedCS >>= BigInteger.ordinal 5I) =! Ok 4I
            
            (bigIntegerCS >>= BigInteger.colour 1I) =! Error (NotUsable "colour")
            (bigIntegerWithedCS >>= BigInteger.colour 1I) =! Ok 2I
            (bigIntegerWithedCS >>= BigInteger.colour 11I) =! Error (OutOfRangeIndex 11I)

            (bigIntegerCS >>= BigInteger.random) =! Error (NotUsable "random")
            
            // let withedCSRandom = (bigIntegerWithedCS >>= BigInteger.random)
            // let toTest = match withedCSRandom with Ok v -> v | Error _ -> -1
            
            // (toTest > 0 && toTest < 11) =! true

            // Check that the function produces actually random variables.
            
        testCase "makeString work as expected for BigInteger" <| fun () ->
            let bigIntegerCS = BigInteger.create None
            let bigIntegerWithedCS = BigInteger.create (Some ("1", "5"))

            (bigIntegerCS >>= BigInteger.makeString "12") =! Ok "12"
            (bigIntegerCS >>= BigInteger.makeString "null") =! Error (InvalidValue "null")

            (bigIntegerWithedCS >>= BigInteger.makeString "12") =! Error (OutOfRangeValue "12")
            (bigIntegerWithedCS >>= BigInteger.makeString "4") =! Ok "4"
            (bigIntegerWithedCS >>= BigInteger.makeString "NaN") =! Error (InvalidValue "NaN")

    ]