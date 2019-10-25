namespace CPN.Simulator.ColorSets

open Common

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
    let create =
        function
        | None -> Ok { unit = "()" }
        | Some unitVal -> Ok { unit = unitVal }
    
    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string value if it is.
    let colorStr supposedMember unitCS = 
        match unitCS, supposedMember with
        | Unit -> Ok supposedMember
        | NonUnit -> Error <| IlegalValue supposedMember

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual converted value if it is.
    let colorVal supposedMember unitCS = 
        match unitCS, supposedMember with
        | Unit -> Ok ()
        | NonUnit -> Error <| IlegalValue supposedMember

    /// Return the default actual converted value for this color set.
    let defaultVal = ()

    /// Return a list of all posible values for this color set.
    let all = Ok [ () ]

    /// Return the number of different vaules in this color set.
    let size = Ok 1
    
    /// Return the ordinal position of every value in this color set.
    let ordinal() = Ok 0

    /// Return the actual convert value for the given position in this color set.
    let colour =
        function
        | 0 -> Ok ()
        | i -> Error <| OutOfRange i

    /// Return a random value of this color set.
    let random() = ()