namespace CPN.Simulator.ColorSets

open Common

type Boolean =
    { falsy: string
      truthy: string }

module Boolean =
    [<AutoOpenAttribute>]
    module private Implementation =
        /// Active pattern for identify Falsy and Truthy values.
        let (|Falsy|Truthy|) (value, booleanCS) = 
            if booleanCS.falsy = value then Falsy else Truthy

        /// Given a value and a color set it return the corresponding actual 
        /// converted value for this set.
        let innerVal value booleanCS =
            match (value, booleanCS) with
            | Falsy -> false
            | Truthy -> true

        /// Given a supposed member and a color set it return if it's an actual 
        /// member of this set.
        let isMember supposedMember booleanCS = 
            supposedMember = booleanCS.falsy || supposedMember = booleanCS.truthy


    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> { falsy = "false"; truthy = "true" }
        | Some(falsyVal, truthyVal) -> { falsy = falsyVal; truthy = truthyVal }

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string value if it is.
    let colorStr supposedMember booleanCS = 
        match isMember supposedMember booleanCS with
        | true -> Ok <| supposedMember
        | false -> Error <| UnexcpectedValue supposedMember

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual converted value if it is.
    let colorVal supposedMember booleanCS =
        match isMember supposedMember booleanCS with
        | true -> Ok <| innerVal supposedMember booleanCS
        | false -> Error <| UnexcpectedValue supposedMember

    /// Return the default actual converted value for this color set.
    let defaultVal = false

    /// Return a list of all posible values for this color set.
    let all = [ false; true ]

    /// Return the number of different vaules in this color set.
    let size = 2

    /// Return the ordinal position of every value in this color set.
    let ordinal = function
        | false -> 0
        | true -> 1
    
    /// Return the actual convert value for the given position in this 
    /// color set.
    let colour = function
        | 0 -> Ok <| false
        | 1 -> Ok <| true
        | i -> Error <| OutOfRange i

    /// Return a random value of this color set.
    let random() = colour (rnd.Next(0,1))