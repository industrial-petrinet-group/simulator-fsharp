#nowarn "025"
namespace CPN.Simulator.Tests.Domain

open Expecto
open Swensen.Unquote

open CPN.Simulator.Operators
open CPN.Simulator.Domain

module MultiSetTests =
    
    let preTest = Setup.declarations()
    
    let unitColour1 = CS "unit"
    let unitColour2 = CS "unit'" 
    let boolColour1 = CS "bool"
    let boolColour2 = CS "bool'"

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

                Ok <| "()" =! ColorSet.serialize unitColour1 value1
                Ok <| "none" =! ColorSet.serialize unitColour2 value2

                ColorSet.deserialize unitColour1 "()" =! 
                ColorSet.deserialize unitColour2 "none"

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
                
                Ok <| "false" =! ColorSet.serialize boolColour1 valueFalse1
                Ok <| "none" =! ColorSet.serialize boolColour2 valueFalse2
                
                ColorSet.deserialize boolColour1 "true" <>! 
                ColorSet.deserialize boolColour2 "none"

                Ok <| Bool false =! ColorSet.deserialize boolColour2 "none"
                
                Ok <| "true" =! ColorSet.serialize boolColour1 valueTrue1
                Ok <| "whole" =! ColorSet.serialize boolColour2 valueTrue2

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

            testCase "adding and removing of MultiSets with ++ and --" <| fun () ->
                let (Ok unitMS1) = MultiSet.ofString unitColour1 "1`()++2`()"
                let (Ok boolMS1) = MultiSet.ofString boolColour1 "1`true++2`false"
                let (Ok boolMS2) = MultiSet.ofString boolColour1 "1`false"
                let (Ok boolMS3) = MultiSet.ofString boolColour1 "1`true++1`false"

                Ok boolMS2 =! (boolMS1 -- boolMS3)
                Ok boolMS1 =! (boolMS2 ++ boolMS3)

                // Can't substract a multiset greater than the substracted one
                Error (MSErrors SubstractorShouldBeLessOrEqual) =! (boolMS3 -- boolMS1)
                // Or With different colors
                let colorList = ["CS \"bool\""; "CS \"unit\""]
                Error (MSErrors (UnmatchedColors colorList)) =! (boolMS1 -- unitMS1)

                let empty = MultiSet.empty 
                let emptyB = MultiSet.emptyWithColor boolColour1

                // Identity, substracting an empty MultiSet returns the same
                Ok boolMS1 =! (boolMS1 -- empty)
                Ok boolMS2 =! (boolMS2 -- emptyB)

                // Substracting itself return the empty color-bounded multiset
                Ok emptyB =! (boolMS1 -- boolMS1)
            
            testCase "scalar multiplication MultiSets with **" <| fun () ->
                let (Ok unitMS1) = MultiSet.ofString unitColour1 "1`()++2`()"
                let (Ok unitMS2) = MultiSet.ofString unitColour1 "4`()++5`()"
                let (Ok boolMS1) = MultiSet.ofString boolColour1 "1`true++2`false"
                let (Ok boolMS2) = MultiSet.ofString boolColour1 "2`true++4`false"

                unitMS2 =! (unitMS1 ** 3)
                boolMS2 =! (boolMS1 ** 2)

                // Identity, multiplying by 1 return the same multiset
                unitMS1 =! (unitMS1 ** 1)
                boolMS1 =! (boolMS1 ** 1)

            testCase "test size, random and colorOcurrence" <| fun () ->
                let (Ok boolMS1) = MultiSet.ofString boolColour1 "2`true++4`false"
                let (Ok unitMS1) = MultiSet.ofString unitColour2 "1`none++1`none++3`none"

                6 =! (boolMS1 |> MultiSet.size)
                5 =! (unitMS1 |> MultiSet.size)

                let empty = MultiSet.empty 
                0 =! (empty |> MultiSet.size)

                let randomBool = [for _ in [1..10] do (boolMS1 |> MultiSet.random)] 
                
                false =! (randomBool |> List.forall (fun b -> b = (randomBool |> List.head)))
                true =! (randomBool |> List.forall (fun b -> b = Bool true || b =  Bool false))

                4 =! (boolMS1 |> MultiSet.colorOcurrences (Bool false))
                2 =! (boolMS1 |> MultiSet.colorOcurrences (Bool true))

                5 =! (unitMS1 |> MultiSet.colorOcurrences Unit)

            testCase "test filtering of MultiSets" <| fun () ->
                let (Ok boolMS1) = MultiSet.ofString boolColour2 "1`none++1`whole++1`none"

                let falsyFilteredMS =
                    boolMS1 
                    |> MultiSet.filter (fun value _ -> 
                        Ok "none" = ColorSet.serialize boolColour2 value)

                "2`none" =! (falsyFilteredMS |> MultiSet.asString)

                let exactlyOneValuesMS =
                    boolMS1 
                    |> MultiSet.filter (fun _ qty -> qty = 1)

                "1`whole" =! (exactlyOneValuesMS |> MultiSet.asString)

            testCase "test map and fold functions for Multiset" <| fun () ->
                let (Ok boolMS1) = MultiSet.ofString boolColour2 "1`none++1`whole++1`none"
                let (Ok boolMS2) = MultiSet.ofString boolColour2 "2`whole++1`none"

                // old ext_col
                let notBoolMS2 =
                    boolMS2 
                    |> MultiSet.mapColors (fun (Bool bool) ->                           
                        bool |> not |> Color.pack |> function Ok b -> b )
                        

                boolMS1 =! notBoolMS2

                // old ext_ms
                let notBoolMS1x2 =
                    boolMS1 
                    |> MultiSet.mapMultiSets (fun (Bool bool) ->
                        bool 
                        |> not
                        |> Color.pack
                        >>= ColorSet.serialize boolColour2
                        >>= fun colorStr -> 
                            MultiSet.ofString boolColour2 (sprintf "2`%s" colorStr)
                        |> function
                            | Ok ms -> ms
                            | Error _ -> MultiSet.empty)
                
                Ok (boolMS2 ** 2) =! notBoolMS1x2

        ]
