namespace CPN.Simulator.Tests.Domain.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets


module BigintCSTests =

    [<Tests>]
    let tests =
        testList "Domain.ColorSets.BigintCSTests." [
            testCase "Bigint color set can be created and it's value for '1353' is 1353" <| fun () ->
                Ok <| Bigint 1353I =! ColorSet.deserialize (CS "bigint") "1353"
            
            //testCase "create and colorVal work as expected for BigInteger" <| fun () ->
            //    let bigIntegerCS = BigInteger.create None
            //    let bigIntegerWithedCS = BigInteger.create (Some ("20", "23452123153426354875151654213215487511"))
                
            //    BigInteger.create (Some ("25", "22")) =! (Error <| CSErrors (InvalidInitialState "low must be less than or equal to high"))
            //    BigInteger.create (Some ("false", "20")) =! (Error <| CSErrors (InvalidInitialState "low and high must be big integer values"))

            //    (bigIntegerCS >>= BigInteger.colorVal "23452123153426354875151654213215487511") 
            //    =! Ok 23452123153426354875151654213215487511I

            //    (bigIntegerCS >>= BigInteger.colorVal "false") =! (Error <| CSErrors (InvalidValue "false"))
            //    (bigIntegerWithedCS >>= BigInteger.colorVal "19") =! (Error <| CSErrors (OutOfRangeValue "19"))


            //testCase "Functions init and legal work as expected for BigInteger" <| fun () ->
            //    let bigIntegerCS = BigInteger.create None
            //    let bigIntegerWithedCS = BigInteger.create (Some ("0", "9"))
                
            //    BigInteger.init =! 0I

            //    (bigIntegerCS >>= switch (BigInteger.isLegal 12I)) =! Ok true
            //    (bigIntegerWithedCS >>= switch (BigInteger.isLegal 4I)) =! Ok true
            //    (bigIntegerWithedCS >>= switch (BigInteger.isLegal 12I)) =! Ok false
                
            //testCase "Small color set functions work as expected for BigInteger" <| fun () ->
            //    let bigIntegerCS = BigInteger.create None
            //    let bigIntegerWithedCS = BigInteger.create (Some ("1", "10"))

            //    (bigIntegerCS >>= BigInteger.all) =! (Error <| CSErrors (NotUsable "all"))
            //    (bigIntegerWithedCS >>= BigInteger.all) =! Ok [1I..10I]

            //    (bigIntegerCS >>= BigInteger.size) =! (Error <| CSErrors (NotUsable "size"))
            //    (bigIntegerWithedCS >>= BigInteger.size) =! Ok 10I

            //    (bigIntegerCS >>= BigInteger.ordinal 5I) =! (Error <| CSErrors (NotUsable "ordinal"))
            //    (bigIntegerWithedCS >>= BigInteger.ordinal 5I) =! Ok 4I
                
            //    (bigIntegerCS >>= BigInteger.color 1I) =! (Error <| CSErrors (NotUsable "color"))
            //    (bigIntegerWithedCS >>= BigInteger.color 1I) =! Ok 2I
            //    (bigIntegerWithedCS >>= BigInteger.color 11I) =! (Error <| CSErrors (OutOfRangeIndex 11I))

            //    (bigIntegerCS >>= BigInteger.random) =! (Error <| CSErrors (NotUsable "random"))
                
            //    // let withedCSRandom = (bigIntegerWithedCS >>= BigInteger.random)
            //    // let toTest = match withedCSRandom with Ok v -> v | Error _ -> -1
                
            //    // (toTest > 0 && toTest < 11) =! true

            //    // Check that the function produces actually random variables.
                
            //testCase "makeString work as expected for BigInteger" <| fun () ->
            //    let bigIntegerCS = BigInteger.create None
            //    let bigIntegerWithedCS = BigInteger.create (Some ("1", "5"))

            //    (bigIntegerCS >>= BigInteger.makeString "12") =! Ok "12"
            //    (bigIntegerCS >>= BigInteger.makeString "null") =! (Error <| CSErrors (InvalidValue "null"))

            //    (bigIntegerWithedCS >>= BigInteger.makeString "12") =! (Error <| CSErrors (OutOfRangeValue "12"))
            //    (bigIntegerWithedCS >>= BigInteger.makeString "4") =! Ok "4"
            //    (bigIntegerWithedCS >>= BigInteger.makeString "NaN") =! (Error <| CSErrors (InvalidValue "NaN"))

        ]