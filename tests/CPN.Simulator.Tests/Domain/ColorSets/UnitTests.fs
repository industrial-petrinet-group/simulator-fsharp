namespace CPN.Simulator.Tests.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets
open CPN.Simulator.Domain.ColorSets.Common

module UnitTests =

    [<Tests>]
    let tests =
        testList "ColorSets.UnitTests." [
            testCase "Unit color set can be created and it's value is ()" <| fun () ->
                Unit.create None >>= Unit.colorVal "()" =! Ok ()
            
            testCase "create and colorVal work as expected for Unit" <| fun () ->
                let unitCS = Unit.create None
                let unitWithedCS = Unit.create (Some "void")

                (unitCS >>= Unit.colorVal "()") =! (unitWithedCS >>= Unit.colorVal "void")
                (unitCS >>= Unit.colorVal "()") =! Ok ()

                (unitCS >>= Unit.colorVal "null") =! (unitWithedCS >>= Unit.colorVal "null")
                (unitCS >>= Unit.colorVal "null") =! Error (InvalidValue "null")

            testCase "Functions init and legal work as expected for Unit" <| fun () ->
                let unitCS = Unit.create None
                let unitWithedCS = Unit.create (Some "no")

                Unit.init =! ()
                
                unitCS >>= switch (Unit.isLegal ()) =! Ok true
                unitWithedCS >>= switch (Unit.isLegal ()) =! Ok true

            testCase "Small colour set functions work as expected for Unit" <| fun () ->
                let unitCS = Unit.create None
                let unitWithedCS = Unit.create (Some "nulo")

                (unitCS >>= Unit.all) =! (unitWithedCS >>= Unit.all)
                (unitCS >>= Unit.all) =! Ok [ () ]

                (unitCS >>= Unit.size) =! (unitWithedCS >>= Unit.size)
                (unitCS >>= Unit.size) =! Ok 1

                (unitCS >>= Unit.ordinal ()) =! (unitWithedCS >>= Unit.ordinal ())
                (unitCS >>= Unit.ordinal ()) =! Ok 0
                
                (unitCS >>= Unit.colour 0) =! (unitWithedCS >>= Unit.colour 0)
                (unitCS >>= Unit.colour 0) =! Ok ()

                (unitCS >>= Unit.colour 1) =! (unitWithedCS >>= Unit.colour 1)
                (unitCS >>= Unit.colour 1) =! Error (OutOfRangeIndex 1)

                (unitCS >>= Unit.random) =! (unitWithedCS >>= Unit.random)
                (unitCS >>= Unit.random) =! Ok ()

            testCase "makeString work as expected for Unit" <| fun () ->
                let unitCS = Unit.create None
                let unitWithedCS = Unit.create (Some "nulo")

                (unitCS >>= Unit.makeString "()") =! Ok "()"
                (unitCS >>= Unit.makeString "nulo") =! Error (InvalidValue "nulo")

                (unitWithedCS >>= Unit.makeString "()") =! Error (InvalidValue "()")
                (unitWithedCS >>= Unit.makeString "nulo") =! Ok "nulo"

        ]