namespace CPN.Simulator.Tests.Domain.ColorSets

open Expecto
open Swensen.Unquote
open CPN.Simulator.Operators
open CPN.Simulator.Domain

module UnitTests =

    //[<Tests>]
    let tests =
        testList "Domain.ColorSets.UnitTests." [
            testCase "Unit color set can be created and it's value is ()" <| fun () ->
               1 =! 1
               ColorSet.deserialize (CS "unit") "()" =! Ok Unit
            
            //testCase "create and Deserialize work as expected for Unit" <| fun () ->
            //    (ColorSet.deserialize (CS "unit") "()") =! (ColorSet.deserialize (CS "unit'")  "none")
            //    Ok <| Unit () =! (ColorSet.deserialize (CS "unit'")  "none")

            //    (ColorSet.deserialize (CS "unit") "null") =! (ColorSet.deserialize (CS "unit'")  "null")
            //    (Error <| CSErrors (InvalidValue "null")) =! (ColorSet.deserialize (CS "unit'")  "null")

            //testCase "Functions init and legal work as expected for Unit" <| fun () ->
            //    let unitCS = UnitCS.create None
            //    let unitWithedCS = UnitCS.create (Some "no")

            //    UnitCS.init =! ()
                
            //    unitCS >>= switch (UnitCS.isLegal ()) =! Ok true
            //    unitWithedCS >>= switch (UnitCS.isLegal ()) =! Ok true

            //testCase "Small color set functions work as expected for Unit" <| fun () ->
            //    let unitCS = UnitCS.create None
            //    let unitWithedCS = UnitCS.create (Some "nulo")

            //    (unitCS >>= UnitCS.all) =! (unitWithedCS >>= UnitCS.all)
            //    (unitCS >>= UnitCS.all) =! Ok [ () ]

            //    (unitCS >>= UnitCS.size) =! (unitWithedCS >>= UnitCS.size)
            //    (unitCS >>= UnitCS.size) =! Ok 1

            //    (unitCS >>= UnitCS.ordinal ()) =! (unitWithedCS >>= UnitCS.ordinal ())
            //    (unitCS >>= UnitCS.ordinal ()) =! Ok 0
                
            //    (unitCS >>= UnitCS.color 0) =! (unitWithedCS >>= UnitCS.color 0)
            //    (unitCS >>= UnitCS.color 0) =! Ok ()

            //    (unitCS >>= UnitCS.color 1) =! (unitWithedCS >>= UnitCS.color 1)
            //    (unitCS >>= UnitCS.color 1) =! (Error <| CSErrors (OutOfRangeIndex 1))

            //    (unitCS >>= UnitCS.random) =! (unitWithedCS >>= UnitCS.random)
            //    (unitCS >>= UnitCS.random) =! Ok ()

            //testCase "makeString work as expected for Unit" <| fun () ->
            //    let unitCS = UnitCS.create None
            //    let unitWithedCS = UnitCS.create (Some "nulo")

            //    (unitCS >>= UnitCS.makeString ()) =! Ok "()"

            //    (unitWithedCS >>= UnitCS.makeString ()) =! Ok "nulo"

        ]