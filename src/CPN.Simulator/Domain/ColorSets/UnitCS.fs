namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

type UnitCSData = { unit: string }

[<StructuredFormatDisplay("UnitCS = {Show}")>]
type UnitCS = 
    | UnitCS of UnitCSData

    interface ColorSet with
        member _.Name = "UnitCS"   

        member _.Init = Unit ()
        
        member this.Deserialize colorString = 
            match this with
            | UnitCS unitCSD when unitCSD.unit = colorString -> Ok <| Unit ()
            | _ -> Error <| CSErrors (InvalidValue colorString)
        
        member this.Serialize colorValue = 
            match colorValue, this with
            | Unit _, UnitCS unitCSD -> Ok unitCSD.unit 
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)  
        
        member _.IsLegal _colorValue = true

        member _.All = Ok [ Unit () ]

        member _.Size = Ok 1 

        member _.Ordinal _colorValue = Ok 0
        
        member _.Color index = 
            match index with
            | 0 -> Ok <| Unit ()
            | i -> Error <| CSErrors (OutOfRangeIndex i)

        member _.Random = Ok <| Unit ()

    member this.Show = Common.asString (this :> ColorSet)


module UnitCS =
    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok <| UnitCS { unit = "()" }
        | Some unitVal -> Ok <| UnitCS { unit = unitVal }


