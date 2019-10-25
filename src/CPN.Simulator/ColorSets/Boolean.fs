namespace CPN.Simulator.ColorSets

open Common

type Boolean =
    { falsy: string
      truthy: string }

module Boolean =
    [<AutoOpenAttribute>]
    module private Implementation =
        /// Active pattern for identify color set cases.
        let (|Falsy|Truthy|Neither|) (value, booleanCS) = 
            match booleanCS.falsy = value, booleanCS.truthy = value with
            | true, _ -> Falsy 
            | _, true -> Truthy
            | _, _ -> Neither


    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok { falsy = "false"; truthy = "true" }
        | Some(falsyVal, truthyVal) when falsyVal <> truthyVal -> 
            Ok { falsy = falsyVal; truthy = truthyVal }
        | Some _ -> 
            Error <| IlegalInitialState "falsy and truthy must be different"
        
    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string value if it is.
    let colorStr supposedMember booleanCS = 
        match (supposedMember, booleanCS) with
        | Neither -> Error <| IlegalValue supposedMember
        | _ -> Ok supposedMember

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual converted value if it is.
    let colorVal supposedMember booleanCS =
        match (supposedMember, booleanCS) with
        | Falsy -> Ok false
        | Truthy -> Ok true
        | Neither -> Error <| IlegalValue supposedMember

    /// Return the default actual converted value for this color set.
    let defaultVal = false

    /// Return a list of all posible values for this color set.
    let all _ = Ok [ false; true ]

    /// Return the number of different vaules in this color set.
    let size _ = Ok 2

    /// Return the ordinal position of every value in this color set.
    let ordinal = function
        | false -> Ok 0
        | true -> Ok 1
    
    /// Return the actual convert value for the given position in this color set.
    let colour = function
        | 0 -> Ok false
        | 1 -> Ok true
        | i -> Error <| OutOfRange i

    /// Return a random value of this color set.
    let random() = colour (rnd.Next(0,1))