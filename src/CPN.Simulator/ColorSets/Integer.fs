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
                | false, _ -> NonInteger
            | low, high -> 
                let lowInt, highInt = int32 low, int32 high
                
                match (Int32.TryParse value) with 
                | true, intVal when intVal >= lowInt && intVal <= highInt -> Integer intVal
                | true, intVal -> OutOfRangeInteger intVal
                | false, _ -> NonInteger

    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok { low = ""; high = "" }
        | Some(lowVal, highVal) -> 
            let (lowBool, _) = Int32.TryParse lowVal
            let (highBool, _) = Int32.TryParse highVal
            
            match lowBool, highBool with
            | true, true -> Ok { low = lowVal; high = highVal }
            | _ -> Error <| IlegalInitialState "low and high must be 32 bits integer values"

     /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string value if it is.
    let colorStr supposedMember booleanCS = 
        match (supposedMember, booleanCS) with
        | Integer _ -> Ok supposedMember
        | OutOfRangeInteger i -> Error <| OutOfRange i
        | NonInteger -> Error <| IlegalValue supposedMember

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual converted value if it is.
    let colorVal supposedMember booleanCS =
        match (supposedMember, booleanCS) with
        | Integer i -> Ok i
        | OutOfRangeInteger i -> Error <| OutOfRange i
        | NonInteger -> Error <| IlegalValue supposedMember

    /// Return the default actual converted value for this color set.
    let defaultVal = 0

    // ########################################
    // Not aplicable for infinite sets; but still applicable for low high ranges

    /// Return a list of all posible values for this color set.
    let all = Error (NotUsable "all")
    /// Return the number of different vaules in this color set.
    let size = Error (NotUsable "size")
    /// Return the ordinal position of every value in this color set.
    let ordinal = fun _ -> Error <| NotUsable "ordinal"
    /// Return the actual convert value for the given position in this color set.
    let colour = fun _ -> Error <| NotUsable "colour"
    /// Return a random value of this color set.
    let random = fun _ -> Error <| NotUsable "random"