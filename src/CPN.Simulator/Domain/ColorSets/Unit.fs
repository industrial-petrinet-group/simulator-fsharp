namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

type UnitCSData = { unit: string }

[<StructuredFormatDisplay("UnitCS = {Show}")>]
type UnitCS = 
    | UnitCS of UnitCSData

    interface IColorSet<unit> with
        member this.MetaData = 
            let (UnitCS unitCSD) = this

            { name = "UnitCS"
              internalType = typeof<unit>
              colorSetHash = hash unitCSD }
    
        member _.Init = fun () -> ()

        member _.IsLegal _colorValue = true

        member this.ColorValue colorString = 
            let (UnitCS unitCSD) = this
            
            match unitCSD.unit = colorString with
            | true -> Ok <| ()
            | false -> Error <| CSErrors (InvalidValue colorString)
    
        member this.ColorString _colorValue = 
            let (UnitCS unitCSD) = this in Ok unitCSD.unit         

        member _.Size = Ok 1
           
        member _.All = Ok [ () ]
        
        member _.Ordinal _colorValue = Ok 0
        
        member _.Color index = 
            match index with
            | 0 -> Ok <| ()
            | i -> Error <| CSErrors (OutOfRangeIndex i)

        member _.Random = Ok <| ()

    member this.Show = Common.asString (this :> IColorSet<unit>)


module UnitCS =
    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok <| UnitCS { unit = "()" }
        | Some unitVal -> Ok <| UnitCS { unit = unitVal }


