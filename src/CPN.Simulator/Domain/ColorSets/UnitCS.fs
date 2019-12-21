namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

type UnitCSData = { unit: string }

[<StructuredFormatDisplay("UnitCS = {Show}")>]
type UnitCS = 
    | UnitCS of UnitCSData

    interface IColorSet with
        member __.Name = "UnitCS"   

        member __.Init = Unit
        
        member this.Deserialize colorString = 
            match this with
            | UnitCS unitCSD when unitCSD.unit = colorString -> Ok <| Unit
            | _ -> Error <| CSErrors (InvalidValue colorString)
        
        member this.Serialize colorValue = 
            match colorValue, this with
            | Unit _, UnitCS unitCSD -> Ok unitCSD.unit 
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)  
        
        member __.IsLegal colorValue = 
            match colorValue with
            | Unit _ -> Ok true
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)

        member __.All = Ok [ Unit ]

        member __.Size = Ok 1 

        member __.Ordinal _colorValue = Ok 0
        
        member __.Color index = 
            match index with
            | 0 -> Ok <| Unit
            | i -> Error <| CSErrors (OutOfRangeIndex i)

        member __.Random = Ok <| Unit

    member this.Show = Common.asString (this :> IColorSet)


module UnitCS =
    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok <| UnitCS { unit = "()" }
        | Some unitVal -> Ok <| UnitCS { unit = unitVal }


