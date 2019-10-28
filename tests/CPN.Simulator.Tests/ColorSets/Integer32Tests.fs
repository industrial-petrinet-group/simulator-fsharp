module Integer32Tests

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.ColorSets
open CPN.Simulator.ColorSets.Common

[<Tests>]
let tests =
    testList "ColorSets/Integer32Tests" [
        testCase "Integer32 color set can be created and it's value for '135' is 135" <| fun () ->
            (Integer32.create None >>= Integer32.colorVal "135") =! Ok 135
        
        testCase "create and colorVal work as expected for Integer32" <| fun () ->
            let integer32CS = Integer32.create None
            let integer32WithedCS = Integer32.create (Some ("20", "25"))
            
            Integer32.create (Some ("25", "20")) =! Error (InvalidInitialState "low must be less than or equal to high")
            Integer32.create (Some ("false", "20")) =! Error (InvalidInitialState "low and high must be 32 bits integer values")

            (integer32CS >>= Integer32.colorVal "23") =! (integer32WithedCS >>= Integer32.colorVal "23")
            (integer32CS >>= Integer32.colorVal "23") =! Ok 23

            (integer32CS >>= Integer32.colorVal "false") =! Error (InvalidValue "false")
            (integer32WithedCS >>= Integer32.colorVal "26") =! Error (OutOfRangeValue "26")


        testCase "Functions init and legal work as expected for Integer32" <| fun () ->
            let integer32CS = Integer32.create None
            let integer32WithedCS = Integer32.create (Some ("0", "9"))
            
            Integer32.init =! 0

            (integer32CS >>= switch (Integer32.isLegal 12)) =! Ok true
            (integer32WithedCS >>= switch (Integer32.isLegal 4)) =! Ok true
            (integer32WithedCS >>= switch (Integer32.isLegal 12)) =! Ok false
            
        testCase "Small colour set functions work as expected for Integer32" <| fun () ->
            let integer32CS = Integer32.create None
            let integer32WithedCS = Integer32.create (Some ("1", "10"))

            (integer32CS >>= Integer32.all) =! Error (NotUsable "all")
            (integer32WithedCS >>= Integer32.all) =! Ok [1..10]

            (integer32CS >>= Integer32.size) =! Error (NotUsable "size")
            (integer32WithedCS >>= Integer32.size) =! Ok 10

            (integer32CS >>= Integer32.ordinal 5) =! Error (NotUsable "ordinal")
            (integer32WithedCS >>= Integer32.ordinal 5) =! Ok 4
            
            (integer32CS >>= Integer32.colour 1) =! Error (NotUsable "colour")
            (integer32WithedCS >>= Integer32.colour 1) =! Ok 2
            (integer32WithedCS >>= Integer32.colour 11) =! Error (OutOfRangeIndex 11)

            (integer32CS >>= Integer32.random) =! Error (NotUsable "random")
            
            let withedCSRandom = (integer32WithedCS >>= Integer32.random)
            let toTest = match withedCSRandom with Ok v -> v | Error _ -> -1
            
            (toTest > 0 && toTest < 11) =! true

            // Check that the function produces actually random variables.
            
        testCase "makeString work as expected for Integer32" <| fun () ->
            let integer32CS = Integer32.create None
            let integer32WithedCS = Integer32.create (Some ("1", "5"))

            (integer32CS >>= Integer32.makeString "12") =! Ok "12"
            (integer32CS >>= Integer32.makeString "null") =! Error (InvalidValue "null")

            (integer32WithedCS >>= Integer32.makeString "12") =! Error (OutOfRangeValue "12")
            (integer32WithedCS >>= Integer32.makeString "4") =! Ok "4"
            (integer32WithedCS >>= Integer32.makeString "NaN") =! Error (InvalidValue "NaN")

    ]