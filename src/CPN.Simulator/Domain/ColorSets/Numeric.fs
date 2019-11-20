namespace CPN.Simulator.Domain.ColorSets
//TODO: makeString should take a value form the CS not a string!!
open CPN.Simulator.Domain

/// Interface for implementing in every Numeric Color Set
// TODO: Better comment this section knowing that it's an abstract one
type INumeric<'T> =
    abstract member Low : 'T
    abstract member High : 'T

/// Module with helper functions for implementing any Numeric Color Set
module Numeric =
    
    /// Auxiliary active pattern for determine if there is a range
    let (|EmptyRange|Range|) ((emptyLow, emptyHigh), (numericCS: INumeric<'T>)) =
        let low, high = numericCS.Low, numericCS.High

        match low, high with 
        | low, high when low = emptyLow && high = emptyHigh -> EmptyRange 
        | _ -> Range (low, high)
    
    /// Active pattern for identify color set cases.
    let (|Number|OutOfRangeNumber|NaN|) (emptyVal, parsedVal, numericCS: INumeric<'T>) = 
        match emptyVal, numericCS with
        | EmptyRange ->
            match parsedVal with
            | true, numVal ->  Number numVal 
            | false, _ -> NaN
        | Range (low, high) ->                 
            match parsedVal with 
            | true, numVal when numVal >= low && numVal <= high -> Number numVal 
            | true, _ -> OutOfRangeNumber
            | false, _ -> NaN

    /// Given an optional initinalization string it return a color set.
    let create typeName emptyVal numParseFunc = function
        | None -> Ok emptyVal
        | Some(lowVal, highVal) -> 
            let (lowBool, lowNum) = numParseFunc lowVal
            let (highBool, highNum) = numParseFunc highVal

            let lowerErrMsg = "low must be less than or equal to high"
            let parseErrMsg = (sprintf "low and high must be %s values" typeName)
            
            match lowBool, highBool, lowNum <= highNum with
            | true, true, true -> Ok (lowNum, highNum)
            | true, true, false -> Error <| CSErrors (InvalidInitialState lowerErrMsg)
            | _ -> Error <| CSErrors (InvalidInitialState parseErrMsg)

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal emptyVal numParseFunc =
        fun supposedMember numberCS -> 
            match (emptyVal, numParseFunc supposedMember, numberCS) with
            | Number i -> Ok i
            | OutOfRangeNumber -> Error <| CSErrors (OutOfRangeValue supposedMember)
            | NaN -> Error <| CSErrors (InvalidValue supposedMember)

    /// Given a value of the type it checks if it's a legal one
    let isLegal emptyVal =
        fun num numericCS ->
            match (emptyVal, (true, num), numericCS) with Number _ -> true | _ -> false

    // /// Given a supposed member and a color set it checks if the value is a 
    // /// member of the set and return it's string color set value if it is.
    let makeString emptyVal numParseFunc =
        fun supposedMember booleanCS ->
            match (emptyVal, numParseFunc supposedMember, booleanCS) with
            | Number _ -> Ok supposedMember
            | OutOfRangeNumber -> Error <| CSErrors (OutOfRangeValue supposedMember)
            | NaN -> Error <| CSErrors (InvalidValue supposedMember)

    /// Return a list of all posible values for this color set.
    let inline all emptyVal = 
        fun numericCS ->
            match (emptyVal, numericCS) with
            | EmptyRange -> Error <| CSErrors (NotUsable "all")
            | Range (low, high) -> Ok [low..high]

    /// Return the number of different vaules in this color set.
    let inline size emptyVal plusUnit = 
        fun numericCS ->
            match (emptyVal, numericCS) with
            | EmptyRange -> Error <| CSErrors (NotUsable "size")
            | Range (low, high) -> Ok (high - low + plusUnit)

    /// Return the ordinal position of every value in this color set.
    let inline ordinal emptyVal = 
        fun num numericCS ->
            match (emptyVal, numericCS) with
            | EmptyRange -> Error <| CSErrors (NotUsable "ordinal")
            | Range(low, high) when num >= low && num <= high -> Ok (num - low)
            | Range _ -> Error <| CSErrors (OutOfRangeValue (string num))

    /// Return the actual value for the given position in this color set.
    let inline colour emptyVal =
        fun num numericCS ->
            match (emptyVal, numericCS) with
            | EmptyRange -> Error <| CSErrors (NotUsable "colour")
            | Range(low, high) when num >= low && num <= high -> Ok (num + low)
            | Range _ -> Error <| CSErrors (OutOfRangeIndex num)
                
                
