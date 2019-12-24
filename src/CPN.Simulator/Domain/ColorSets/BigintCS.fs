namespace CPN.Simulator.Domain.ColorSets

open System
open CPN.Simulator.Domain
open CPN.Simulator.Operators

/// Type representing the Bigint ColorSet Data
type BigintCSData = 
    { low: bigint
      high: bigint }

/// Type representing a Bigint ColorSet
[<StructuredFormatDisplay("BigintCS = {Show}")>]
type BigintCS =
    | BigintCS of BigintCSData 
    
    interface INumeric<bigint> with
        member this.Low = let (BigintCS bicsd) = this in bicsd.low
        member this.High = let (BigintCS bicsd) = this in bicsd.high 
        member __.TypeName = "big integer"
        member __.EmptyValue = (1I, 0I)
        member __.Parse value = bigint.TryParse value
    
    interface IColorSet with
        member __.Name = "BigintCS"
    
        member __.Init = Bigint 0I
    
        member this.Deserialize colorString = 
            this |> Numeric.deserialize colorString >>= Color.pack
                
        member this.Serialize colorValue = 
            this |> Numeric.serialize (colorValue |> Color.unpack)
    
        member this.IsLegal colorValue =
            match colorValue with 
            | Bigint bigintColor -> Ok <| (this |> Numeric.isLegal bigintColor)
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)
    
        member this.All =  this |> Numeric.all
       
        member this.Size = this |> Numeric.size

        member this.Color index = this |> Numeric.color index 
    
        member this.Ordinal colorValue = this |> Numeric.ordinal colorValue
    
        member __.Random = Error <| CSErrors (NotUsable "random") // Check for future implementations
    
    member this.Show = Common.asString (this :> IColorSet)


module BigintCS =
    /// return the empty Big Integer CS
    let empty = BigintCS { low = 1I; high = 0I }   
    
    /// Given an optional initinalization string it return a color set.
    let create lowAndHigh = 
        match Numeric.create empty lowAndHigh with
        | Ok (lowVal, highVal) -> Ok { low = lowVal; high = highVal }
        | Error err -> Error err 