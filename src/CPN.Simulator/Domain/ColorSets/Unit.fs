namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

type Unit =
    { unit: string }

module Unit =

    [<AutoOpenAttribute>]
    module private Implementation =        
        /// Active pattern for identify color set cases.
        let (|Unit|NonUnit|) (unitCS, value) = 
            match unitCS.unit = value with
            | true -> Unit 
            | _ -> NonUnit

    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok { unit = "()" }
        | Some unitVal -> Ok { unit = unitVal }

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal supposedMember unitCS = 
        match unitCS, supposedMember with
        | Unit -> Ok ()
        | NonUnit -> Error <| CSErrors (InvalidValue supposedMember)

    /// Return the base initial value for this color set.
    let init = ()

    /// Given a value of the type it checks if it's a legal one
    let isLegal () (_: Unit) = true

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let makeString supposedMember unitCS = 
        match isLegal supposedMember unitCS with
        | false -> Error <| CSErrors (InvalidValue (sprintf "%A" supposedMember))
        | true -> Ok unitCS.unit 

    /// Return a list of all posible values for this color set.
    let all (_: Unit) = Ok [ () ]

    /// Return the number of different vaules in this color set.
    let size (_: Unit) = Ok 1
    
    /// Return the ordinal position of every value in this color set.
    let ordinal () (_: Unit) = Ok 0

    /// Return the actual value for the given position in this color set.
    let color i (_: Unit) =
        match i with
        | 0 -> Ok ()
        | i -> Error <| CSErrors (OutOfRangeIndex i)

    /// Return a random value of this color set.
    let random (_: Unit) = Ok ()

    /// Return a string representing the Color Set
    let asString unitCS = 
        let allValues = 
            match (all unitCS) with
            | Error _ -> ""
            | Ok list -> sprintf "%A" list

        sprintf "Unit = %s" allValues