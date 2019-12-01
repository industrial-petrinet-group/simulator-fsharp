#nowarn "025"
namespace CPN.Simulator.Tests.Domain

open Expecto
open Swensen.Unquote

open CPN.Simulator.Operators
open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets

module MultiSetTests =
    
    let (Ok unitCS1) = Unit.create None
    let (Ok unitCS2) = Unit.create (Some "none")
    let unitColour1, unitColour2 = UnitCS unitCS1, UnitCS unitCS2
    
    let (Ok boolCS1) = Boolean.create None
    let (Ok boolCS2) = Boolean.create (Some ("none", "whole"))
    let boolColour1, boolColour2 = BooleanCS boolCS1, BooleanCS boolCS2

    [<Tests>]
    let tests = 
         testList "Domain.MultiSet." [
            testCase "creation of a unit MultiSet using the string format" <| fun () ->
                let unitMSStr1, unitMSStr2 = "1`()++2`()", "1`none++1`none++1`none"

                let (Ok (MS { values = set1 })) = MultiSet.ofString unitColour1 unitMSStr1
                let (Ok (MS { values = set2 })) = MultiSet.ofString unitColour2 unitMSStr2

                let (value1, qty1) = set1 |> Map.toList |> List.head
                let (value2, qty2) = set2 |> Map.toList |> List.head

                3 =! qty1
                qty1 =! qty2

                Ok <| UnitVal () =! ColorSet.colorVal value1 unitColour1

                ColorSet.colorVal value1 unitColour1 =! 
                ColorSet.colorVal value2 unitColour2

                let (Ok msAsString) = 
                    unitMSStr1 
                    |> MultiSet.ofString unitColour1 
                    >>= fun multiSet -> Ok (multiSet |> MultiSet.asString)

                "3`()" =! msAsString
                

            testCase "creation of a boolean MultiSet using the string format" <| fun () ->
                let boolMSStr1, boolMSStr2 = "1`true++2`false", "1`none++1`whole++1`none"

                let (Ok (MS { values = set1 })) = MultiSet.ofString boolColour1 boolMSStr1
                let (Ok (MS { values = set2 })) = MultiSet.ofString boolColour2 boolMSStr2

                let [ (valueFalse1, qtyFalse1); (valueTrue1, qtyTrue1) ] = set1 |> Map.toList
                let [ (valueFalse2, qtyFalse2); (valueTrue2, qtyTrue2) ] = set2 |> Map.toList
                
                (2, 1) =! (qtyFalse1, qtyTrue1)
                (qtyFalse1, qtyTrue1) =! (qtyFalse2, qtyTrue2) 
                
                Ok <| BooleanVal false =! 
                ColorSet.colorVal valueFalse1 boolColour1
                
                ColorSet.colorVal valueFalse1 boolColour1 =! 
                ColorSet.colorVal valueFalse2 boolColour2

                Ok <| BooleanVal true =! 
                ColorSet.colorVal valueTrue1 boolColour1
                
                ColorSet.colorVal valueTrue1 boolColour1 =! 
                ColorSet.colorVal valueTrue2 boolColour2

                let (Ok msAsString) = 
                    boolMSStr1 
                    |> MultiSet.ofString boolColour1 
                    >>= fun multiSet -> Ok (multiSet |> MultiSet.asString)

                "1`true++2`false" =! msAsString
            
            testCase "equality and inequality of MultiSets" <| fun () ->
                let unitMSStr1, unitMSStr2 = "1`()++2`()", "1`()++1`()++1`()"
                
                let boolMSStr1, boolMSStr2, boolMSStr3 = 
                    "1`true++2`false", "1`false++1`true++1`false", "1`true++1`false"

                let (Ok msUnit1) = MultiSet.ofString unitColour1 unitMSStr1
                let (Ok msUnit2) = MultiSet.ofString unitColour1 unitMSStr2

                let (Ok msBool1) = MultiSet.ofString boolColour1 boolMSStr1
                let (Ok msBool2) = MultiSet.ofString boolColour1 boolMSStr2
                let (Ok msBool3) = MultiSet.ofString boolColour1 boolMSStr3

                true =! (msUnit1 = msUnit2)
                true =! (msBool1 = msBool2)

                false =! (msUnit1 = msBool1)
                false =! (msBool1 = msBool3)

                true =! (msUnit1 <> msBool1) 

                let (Ok ms1AsString) = 
                    boolMSStr1 
                    |> MultiSet.ofString boolColour1 
                    >>= fun multiSet -> Ok (multiSet |> MultiSet.asString)

                let (Ok ms2AsString) = 
                    boolMSStr2 
                    |> MultiSet.ofString boolColour1 
                    >>= fun multiSet -> Ok (multiSet |> MultiSet.asString)

                "1`true++2`false" =! ms1AsString
                ms1AsString =! ms2AsString

            testCase "comparisson of MultiSets" <| fun () ->            
                let (Ok unitMS1) = MultiSet.ofString unitColour1 "1`()++2`()"
                let (Ok unitMS2) = MultiSet.ofString unitColour1 "1`()++1`()++1`()"
                let (Ok unitMS3) = MultiSet.ofString unitColour1 "1`()++1`()"

                true =! (unitMS1 > unitMS3)
                true =! (unitMS1 >= unitMS2)
                true =! (unitMS2 >= unitMS1)

                false =! (unitMS3 > unitMS2)
                false =! (unitMS2 < unitMS1)
                
                let (Ok boolMS1) = MultiSet.ofString boolColour1 "1`true++2`false"
                let (Ok boolMS2) = MultiSet.ofString boolColour2 "1`none++1`whole++1`none"
                let (Ok boolMS3) = MultiSet.ofString boolColour1 "1`true++1`false"
                let (Ok boolMS4) = MultiSet.ofString boolColour1 "1`false++1`true++1`false"
                let (Ok boolMS5) = MultiSet.ofString boolColour2 "2`none++1`whole"
                let (Ok boolMS6) = MultiSet.ofString boolColour2 "1`none"
                let (Ok boolMS7) = MultiSet.ofString boolColour2 "1`whole"

                true =! ((MultiSet.emptyWithColor boolColour1) < boolMS1)
                true =! (boolMS3 < boolMS1)
                true =! (boolMS4 <= boolMS1)
                true =! (boolMS1 <= boolMS4)
                true =! (boolMS2 >= boolMS5)
                true =! (boolMS5 > boolMS6)

                false =! (boolMS1 < boolMS4)
                false =! (boolMS2 > boolMS5)
                false =! (boolMS6 > boolMS5)
                
                //cannot compare different colors
                raises<System.ArgumentException> <@ boolMS1 > unitMS1 @>
                raises<System.ArgumentException> <@ boolMS1 > boolMS2 @>

                //cannot compare disjoint multisets
                raises<System.ArgumentException> <@ boolMS7 > boolMS6 @>

            testCase "comparisson of empty MultiSets and checking emptyness" <| fun () ->
                let (Ok unitMS1) = MultiSet.ofString unitColour1 "1`()++2`()"
                let (Ok boolMS1) = MultiSet.ofString boolColour1 "1`true++2`false"

                let empty = MultiSet.empty 
                let emptyU = MultiSet.emptyWithColor unitColour1
                let emptyB = MultiSet.emptyWithColor boolColour1
                
                // Can't compare empty color-bounded MultiSets given that they 
                // have different color
                raises<System.ArgumentException> <@ emptyU < boolMS1 @>
                raises<System.ArgumentException> <@ emptyB < unitMS1 @>

                true =! MultiSet.isEmpty empty
                true =! MultiSet.isEmpty emptyU
                true =! MultiSet.isEmpty emptyB

                // not bounded empty multisets can be compared with any multiset
                // bounded only with it's color
                true =! (empty < unitMS1)
                true =! (empty < boolMS1)
                true =! (emptyU < unitMS1)
                true =! (emptyB < boolMS1)

         ]
