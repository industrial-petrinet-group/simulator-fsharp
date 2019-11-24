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

                let (Ok { set = set1 }) = MultiSet.ofString unitColour1 unitMSStr1
                let (Ok { set = set2 }) = MultiSet.ofString unitColour2 unitMSStr2

                let { qty = qty1; value = value1 } = set1 |> Set.toList |> List.head
                let { qty = qty2; value = value2 } = set2 |> Set.toList |> List.head

                3 =! qty1
                qty1 =! qty2

                Ok <| UnitVal () =! ColorSet.colorVal value1 unitColour1

                ColorSet.colorVal value1 unitColour1 =! 
                ColorSet.colorVal value2 unitColour2

                let (Ok msAsString) = 
                    unitMSStr1 
                    |> MultiSet.ofString unitColour1 
                    >>= fun multiSet -> Ok (multiSet |> MultiSet.setAsString)

                "3`()" =! msAsString
                

            testCase "creation of a boolean MultiSet using the string format" <| fun () ->
                let boolMSStr1, boolMSStr2 = "1`true++2`false", "1`none++1`whole++1`none"

                let (Ok { set = set1 }) = MultiSet.ofString boolColour1 boolMSStr1
                let (Ok { set = set2 }) = MultiSet.ofString boolColour2 boolMSStr2

                let [ { qty = qtyTrue1; value = valueTrue1 };
                      { qty = qtyFalse1; value = valueFalse1 } ] = set1 |> Set.toList
                let [ { qty = qtyTrue2; value = valueTrue2 };
                      { qty = qtyFalse2; value = valueFalse2 } ] = set2 |> Set.toList
                
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
                    >>= fun multiSet -> Ok (multiSet |> MultiSet.setAsString)

                "2`false++1`true" =! msAsString

         ]
