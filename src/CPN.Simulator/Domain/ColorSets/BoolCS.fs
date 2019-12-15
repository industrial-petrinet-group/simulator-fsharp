namespace CPN.Simulator.Domain.ColorSets

open Common
open CPN.Simulator.Domain

type BooleanCSData =
    { falsy: string
      truthy: string }

type BoolCS = 
    | BoolCS of BooleanCSData

    interface IColorSet with
        member __.Name = "BooleanCS"
        
        member __.Init = Bool false
        
        member this.Deserialize colorString = 
            let (BoolCS booleanCSD) = this
            let falsyOrTruthy = booleanCSD.falsy = colorString,
                                booleanCSD.truthy = colorString

            match falsyOrTruthy with
            | true, _ -> Ok <| Bool false
            | _, true -> Ok <| Bool true
            | false, false -> Error <| CSErrors (InvalidValue colorString)
        
        member this.Serialize colorValue = 
            match colorValue, this with
            | Bool bool, BoolCS boolCSD -> 
                Ok <| if bool then boolCSD.truthy else boolCSD.falsy
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)
        
        member __.IsLegal colorValue = 
            match colorValue with
            | Bool _ -> Ok true
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)

        member __.All = Ok [ Bool false; Bool true ]
       
        member __.Size = Ok 2

        member __.Color index = 
            match index with
            | 0 -> Ok <| Bool false
            | 1 -> Ok <| Bool true
            | i -> Error <| CSErrors (OutOfRangeIndex i)
        
        member __.Ordinal colorValue = 
            match colorValue with 
            | Bool false -> Ok 0 
            | Bool true -> Ok 1
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)
        
        member this.Random = (this :> IColorSet).Color (rnd.Next(0,1))
        
    member this.Show = Common.asString (this :> IColorSet)


module BoolCS =
    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok <| BoolCS { falsy = "false"; truthy = "true" }
        | Some(falsyValue, truthyValue) when falsyValue <> truthyValue -> 
            Ok <| BoolCS { falsy = falsyValue; truthy = truthyValue }
        | Some _ -> 
            Error <| CSErrors (InvalidInitialState "falsy and truthy must be different")