namespace CPN.Simulator.ColorSets

open System
open Common

type Integer32 =
    { low: int32
      high: int32 }

module Integer32 =
    [<AutoOpenAttribute>]
    module private Implementation =
        /// Auxiliary active pattern for determine if there is a range
        let (|EmptyRange|Range|) (integerCS) =
            let low, high = integerCS.low, integerCS.high

            match low, high with (1, 0) -> EmptyRange | _ -> Range (low, high)
        
        /// Active pattern for identify color set cases.
        let (|Integer|OutOfRangeInteger|NonInteger|) (value, integerCS) = 
            match integerCS with
            | EmptyRange -> 
                match (Int32.TryParse value) with
                | true, intVal ->  Integer intVal 
                | false, _ -> NonInteger
            | Range (low, high) ->                 
                match (Int32.TryParse value) with 
                | true, intVal when intVal >= low && intVal <= high -> Integer intVal
                | true, intVal -> OutOfRangeInteger intVal
                | false, _ -> NonInteger

    /// Given an optional initinalization string it return a color set.
    let create = function
        | None -> Ok { low = 1; high = 0 }
        | Some(lowVal, highVal) -> 
            let (lowBool, lowInt) = Int32.TryParse lowVal
            let (highBool, highInt) = Int32.TryParse highVal
            
            match lowBool, highBool, lowInt <= highInt with
            | true, true, true -> Ok { low = lowInt; high = highInt }
            | true, true, false -> Error <| InvalidInitialState "low must be less than or equal to high"
            | _ -> Error <| InvalidInitialState "low and high must be 32 bits integer values"

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal supposedMember booleanCS =
        match (supposedMember, booleanCS) with
        | Integer i -> Ok i
        | OutOfRangeInteger i -> Error <| OutOfRange i
        | NonInteger -> Error <| InvalidValue supposedMember

    /// Return the default actual value for this color set.
    let init = 0

    /// Given a value of the type it checks if it's a legal one
    let isLegal (i: int32) integer32CS = 
        match (i.ToString(), integer32CS) with Integer _ -> true | _ -> false

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let makeString supposedMember booleanCS = 
        match (supposedMember, booleanCS) with
        | Integer _ -> Ok supposedMember
        | OutOfRangeInteger i -> Error <| OutOfRange i
        | NonInteger -> Error <| InvalidValue supposedMember

    /// Return a list of all posible values for this color set.
    let all = function
        | EmptyRange -> Error (NotUsable "all")
        | Range (low, high) -> Ok [low..high]

    /// Return the number of different vaules in this color set.
    let size = function
        | EmptyRange -> Error (NotUsable "size")
        | Range (low, high) -> Ok (high - low + 1)

    /// Return the ordinal position of every value in this color set.
    let ordinal i = function
        | EmptyRange -> Error <| NotUsable "ordinal"
        | Range(low, high) when i >= low && i <= high -> Ok (i - low)
        | Range _ -> Error <| OutOfRange i

    /// Return the actual value for the given position in this color set.
    let colour i = function
        | EmptyRange -> Error <| NotUsable "colour"
        | Range(low, high) when i >= low && i <= high -> Ok (i + low)
        | Range _ -> Error <| OutOfRange i

    /// Return a random value of this color set.
    let random = function
        | EmptyRange -> Error <| NotUsable "random"
        | Range(low, high) -> Ok <| rnd.Next(low, high)