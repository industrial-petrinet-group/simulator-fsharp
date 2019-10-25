namespace CPN.Simulator.ColorSets

open Common

type Unit =
    { unit: string }

module Unit =

    [<AutoOpenAttribute>]
    module private Implementation =        
        /// Active pattern for identify color set cases.
        let (|Unit|NotUnit|) (unitCS, value) = 
            match unitCS.unit = value with
            | true -> Unit 
            | _ -> NotUnit


    /// Given an optional initinalization string it return a color set.
    let create =
        function
        | None -> { unit = "()" }
        | Some unitVal -> { unit = unitVal }
    
    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string value if it is.
    let colorStr supposedMember unitCS = 
        match unitCS, supposedMember with
        | Unit -> Ok <| supposedMember
        | NotUnit -> Error <| IlegalValue supposedMember

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual converted value if it is.
    let colorVal supposedMember unitCS = 
        match unitCS, supposedMember with
        | Unit -> Ok <| ()
        | NotUnit -> Error <| IlegalValue supposedMember

    /// Return the default actual converted value for this color set.
    let defaultVal = ()

    /// Return a list of all posible values for this color set.
    let all = [ () ]

    /// Return the number of different vaules in this color set.
    let size = 1
    
    /// Return the ordinal position of every value in this color set.
    let ordinal() = 0

    /// Return the actual convert value for the given position in this 
    /// color set.
    let colour =
        function
        | 0 -> Ok <| ()
        | i -> Error <| OutOfRange i

    /// Return a random value of this color set.
    let random() = ()