namespace CPN.Simulator.Domain.ColorSets

open System
open CPN.Simulator.Domain
open CPN.Simulator.Operators

type FloatCSData =
    { low: double
      high: double }

[<StructuredFormatDisplay("FloatCS = {Show}")>]
type FloatCS =
    | FloatCS of FloatCSData
      
    interface INumeric<float> with
        member this.Low = let (FloatCS fcsd) = this in fcsd.low
        member this.High = let (FloatCS fcsd) = this in fcsd.high 
        member __.TypeName = "float (64 bits)"
        member __.EmptyValue = (1.0, 0.0)
        member __.Parse value = Double.TryParse value 
    
    interface IColorSet with
        member __.Name = "FloatCS"
    
        member __.Init = Float 0.0
    
        member this.Deserialize colorString = 
            this |> Numeric.deserialize colorString >>= Color.pack
                
        member this.Serialize colorValue = 
            this |> Numeric.serialize (colorValue |> Color.unpack)
    
        member this.IsLegal colorValue =
            match colorValue with 
            | Float floatColor -> Ok <| (this |> Numeric.isLegal floatColor)
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)
        
        //#########################################################################
        // The next functions are not available for Doubles given the infinite 
        // number of memebers between any given two.
        member __.All = Error <| CSErrors (NotUsable "all")
        member __.Size = Error <| CSErrors (NotUsable "size")
        member __.Color _ = Error <| CSErrors (NotUsable "color")
        member __.Ordinal colorValue = Error <| CSErrors (NotUsable "ordinal")
        member __.Random = Error <| CSErrors (NotUsable "random") // Check for future implementations
    
    member this.Show = Common.asString (this :> IColorSet)


module FloatCS =
    /// Return the default actual value for this color set.
    let empty = FloatCS { low = 1.0; high = 0.0 }
    
    /// Given an optional initinalization string it return a color set.
    let create lowAndHigh = 
        match Numeric.create empty lowAndHigh with
        | Ok (lowVal, highVal) -> Ok { low = lowVal; high = highVal }
        | Error err -> Error err      