namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

type UnitCSData = { unit: string }

type UnitCS = 
    | UnitCS of UnitCSData

    interface IColorSet<unit> with
        member _.MetaData = { name = "Unit"; internalType = ().GetType() }
    
        member _.Init = fun () -> ()

        member _.IsLegal _colorValue = true

        member this.ColorValue colorString = 
            let (UnitCS unitCSD) = this
            
            match unitCSD.unit = colorString with
            | true -> Ok <| ()
            | false -> Error <| CSErrors (InvalidValue colorString)
    
        member this.ColorString _colorValue = 
            let (UnitCS unitCSD) = this in Ok unitCSD.unit 
      
        member this.Random = Ok <| ()

        member _.Size = Ok 1
           
        member _.All = Ok [ () ]
        
        member _.Ordinal _colorValue = Ok 0
        
        member _.Color index = 
            match index with
            | 0 -> Ok <| ()
            | i -> Error <| CSErrors (OutOfRangeIndex i)

        
    
    member this.Show = Common.asString (this :> IColorSet<unit>)


module UnitCS =
    [<AutoOpenAttribute>]
    module private Implementation =        
        /// Active pattern for identify color set cases.
        let (|Unit|NonUnit|) (UnitCS unitCS, value) = 
            match unitCS.unit = value with
            | true -> Unit 
            | _ -> NonUnit

    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok <| UnitCS { unit = "()" }
        | Some unitVal -> Ok <| UnitCS { unit = unitVal }

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorValue colorString unitCS = 
        match unitCS, colorString with
        | Unit -> Ok <| ()
        | NonUnit -> Error <| CSErrors (InvalidValue colorString)

    /// Return the base initial value for this color set.
    let init = ()

    /// Given a value of the type it checks if it's a legal one
    let isLegal () (_: UnitCS) = true

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let colorString _colorValue (UnitCS unitCSData) = Ok unitCSData.unit 

    /// Return a list of all posible values for this color set.
    let all (_: UnitCS) = Ok [ () ]

    /// Return the number of different vaules in this color set.
    let size (_: UnitCS) = Ok 1
    
    /// Return the ordinal position of every value in this color set.
    let ordinal () (_: UnitCS) = Ok 0

    /// Return the actual value for the given position in this color set.
    let color i (_: UnitCS) =
        match i with
        | 0 -> Ok <| ()
        | i -> Error <| CSErrors (OutOfRangeIndex i)

    /// Return a random value of this color set.
    let random (_: UnitCS) = Ok <| ()



