namespace CPN.Simulator.Domain.ColorSets

open System
open CPN.Simulator.Domain
open CPN.Simulator.Operators

/// Type representing the Int ColorSet Data
type IntCSData = 
    { low: int
      high: int }

/// Type representing the Int ColorSet
[<StructuredFormatDisplay("IntCS = {Show}")>]
type IntCS =    
    | IntCS of IntCSData
     
    interface INumeric<int> with
        member this.Low = let (IntCS icsd) = this in icsd.low
        member this.High = let (IntCS icsd) = this in icsd.high 
        member __.TypeName = "integer (32 bits)"
        member __.EmptyValue = (1, 0)
        member __.Parse value = Int32.TryParse value

    interface IColorSet with
        member __.Name = "IntCS"
        
        member __.Init = Int 0
        
        member this.Deserialize colorString = 
            this |> Numeric.deserialize colorString
                    
        member this.Serialize colorValue = 
            this |> Numeric.serialize colorValue
        
        member this.IsLegal colorValue =
            match colorValue with 
            | Int intColor -> Ok <| (this |> Numeric.isLegal intColor)
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)
        
        member this.All =  this |> Numeric.all
       
        member this.Size = this |> Numeric.size

        member this.Color index = this |> Numeric.color index 
        
        member this.Ordinal colorValue = this |> Numeric.ordinal colorValue
        
        member __.Random = Error <| CSErrors (NotUsable "random") // Check for future implementations
        
    member this.Show = Common.asString (this :> IColorSet)


module IntCS =
    /// return the empty Integer CS
    let empty = IntCS { low = 1; high = 0}
    
    /// Given an optional initinalization string it return a color set.
    let create lowAndHigh = 
        match Numeric.create empty lowAndHigh with
        | Ok (lowVal, highVal) -> Ok <| IntCS { low = lowVal; high = highVal }
        | Error err -> Error err      