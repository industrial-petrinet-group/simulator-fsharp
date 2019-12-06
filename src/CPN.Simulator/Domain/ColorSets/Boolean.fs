namespace CPN.Simulator.Domain.ColorSets

open Common
open CPN.Simulator.Domain

type BooleanCSData =
    { falsy: string
      truthy: string }

type BooleanCS = 
    | BooleanCS of BooleanCSData

    interface IColorSet<bool> with
        member this.MetaData = 
            let (BooleanCS booleanCSD) = this

            { name = "BooleanCS"
              internalType = typeof<bool> 
              colorSetHash = hash booleanCSD }
        
        member _.Init = fun () -> false
        
        member _.Size = Ok 2
        
        member _.All = Ok [ false; true ]
       
        member _.IsLegal _colorValue = true

        member _.Color index = 
            match index with
            | 0 -> Ok false
            | 1 -> Ok true
            | i -> Error <| CSErrors (OutOfRangeIndex i)
        
        member _.Ordinal colorValue = 
            match colorValue with 
            | false -> Ok 0 
            | true -> Ok 1
        
        member this.Random = (this :> IColorSet<bool>).Color (rnd.Next(0,1))

        member this.ColorValue colorString = 
            let (BooleanCS booleanCSD) = this
            let falsyOrTruthy = booleanCSD.falsy = colorString,
                                booleanCSD.truthy = colorString

            match falsyOrTruthy with
            | true, _ -> Ok false
            | _, true -> Ok true
            | false, false -> Error <| CSErrors (InvalidValue colorString)
                   
        member this.ColorString colorValue = 
            let (BooleanCS booleanCSD) = this

            match (this :> IColorSet<bool>).IsLegal colorValue with
            | false -> Error <| CSErrors (InvalidValue (sprintf "%A" colorValue))
            | true -> Ok <| if colorValue then booleanCSD.truthy 
                                          else booleanCSD.falsy
        
    member this.Show = Common.asString (this :> IColorSet<bool>)


module BooleanCS =
    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok <| BooleanCS { falsy = "false"; truthy = "true" }
        | Some(falsyVal, truthyVal) when falsyVal <> truthyVal -> 
            Ok <| BooleanCS { falsy = falsyVal; truthy = truthyVal }
        | Some _ -> 
            Error <| CSErrors (InvalidInitialState "falsy and truthy must be different")