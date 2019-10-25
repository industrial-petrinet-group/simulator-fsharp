namespace CPN.Simulator.ColorSets

open System
open Common

type Integer32 =
    { low: string
      high: string }

module Integer32 =
    [<AutoOpenAttribute>]
    module private Implementation =
        /// Active pattern for identify color set cases.
        let (|Integer|OutOfRangeInteger|NonInteger|) (value, integerCS) = 
            match integerCS.low, integerCS.high with
            | "", "" -> 
                match (Int32.TryParse value) with
                | true, intVal ->  Integer intVal 
                | false, _ -> NonInteger value
            | low, high -> 
                let lowInt, highInt = int32 low, int32 high
                
                match (Int32.TryParse value) with 
                | true, intVal when intVal >= lowInt && intVal <= highInt -> Integer intVal
                | true, intVal -> OutOfRangeInteger intVal
                | false, _ -> NonInteger value

    // /// Given an optional initinalization string it return a color set.
    // let create = function
    //     | None -> { falsy = "false"; truthy = "true" }
    //     | Some(falsyVal, truthyVal) -> { falsy = falsyVal; truthy = truthyVal }

    // /// Given a supposed member and a color set it checks if the value is a 
    // /// member of the set and return it's string value if it is.
    // let colorStr supposedMember booleanCS = 
    //     match isMember supposedMember booleanCS with
    //     | true -> Ok <| supposedMember
    //     | false -> Error <| UnexcpectedValue supposedMember

    // /// Given a supposed member and a color set it checks if the value is a 
    // /// member of the set and return it's actual converted value if it is.
    // let colorVal supposedMember booleanCS =
    //     match isMember supposedMember booleanCS with
    //     | true -> Ok <| innerVal supposedMember booleanCS
    //     | false -> Error <| UnexcpectedValue supposedMember

    // /// Return the default actual converted value for this color set.
    // let defaultVal = false

    // /// Return a list of all posible values for this color set.
    // let all = [ false; true ]

    // /// Return the number of different vaules in this color set.
    // let size = 2

    // /// Return the ordinal position of every value in this color set.
    // let ordinal = function
    //     | false -> 0
    //     | true -> 1
    
    // /// Return the actual convert value for the given position in this 
    // /// color set.
    // let colour = function
    //     | 0 -> Ok <| false
    //     | 1 -> Ok <| true
    //     | i -> Error <| OutOfRange i

    // /// Return a random value of this color set.
    // let random() = colour (rnd.Next(0,1))