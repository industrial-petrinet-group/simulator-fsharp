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
            Error <| InvalidInitialState "falsy and truthy must be different"
        
    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal supposedMember booleanCS =
        match (supposedMember, booleanCS) with
        | Falsy -> Ok false
        | Truthy -> Ok true
        | Neither -> Error <| InvalidValue supposedMember

    /// Return the default actual value for this color set.
    let init = false

    /// Given a value of the type it checks if it's a legal one
    let legal (_: bool) = true

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let makeString supposedMember booleanCS = 
        match (supposedMember, booleanCS) with
        | Neither -> Error <| InvalidValue supposedMember
        | _ -> Ok supposedMember

    /// Return a list of all posible values for this color set.
    let all (_: Boolean) = Ok [ false; true ]

    /// Return the number of different vaules in this color set.
    let size (_: Boolean) = Ok 2

    /// Return the ordinal position of every value in this color set.
    let ordinal b (_: Boolean) =
        match b with
        | false -> Ok 0
        | true -> Ok 1
    
    /// Return the actual value for the given position in this color set.
    let colour i (_: Boolean) =
        match i with
        | 0 -> Ok false
        | 1 -> Ok true
        | i -> Error <| OutOfRange i

    /// Return a random value of this color set.
    let random booleanCS = colour (rnd.Next(0,1)) booleanCS