namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain
open CPN.Simulator.Operators

/// Interface for implementing in every Numeric Color Set
// TODO: Better comment this section knowing that it's an abstract one
type INumeric<'T> =
    abstract member Low : 'T
    abstract member High : 'T
    abstract member TypeName : string
    abstract member EmptyValue : 'T * 'T
    abstract member Parse : string -> bool * 'T

/// Module with helper functions for implementing any Numeric Color Set
module Numeric =
    
    /// Auxiliary active pattern for determine if there is a range
    let (|EmptyRange|Range|) ((emptyLow, emptyHigh), (numericCS: INumeric<'T>)) =
        let low, high = numericCS.Low, numericCS.High

        match low, high with 
        | low, high when low = emptyLow && high = emptyHigh -> EmptyRange 
        | _ -> Range (low, high)
    
    /// Active pattern for identify color set cases.
    let (|Number|OutOfRangeNumber|NaN|) (parsed, numericCS: INumeric<'T>) = 
        match numericCS.EmptyValue, numericCS with
        | EmptyRange ->
            match parsed with
            | true, numVal ->  Number numVal 
            | false, _ -> NaN
        | Range (low, high) ->                 
            match parsed with 
            | true, numVal when numVal >= low && numVal <= high -> Number numVal 
            | true, _ -> OutOfRangeNumber
            | false, _ -> NaN

    /// Given an optional initinalization string it return a color set.
    let create (empty : INumeric<'T>) = function
        | None -> Ok empty.EmptyValue
        | Some(lowVal, highVal) -> 
            let (lowBool, lowNum) = empty.Parse lowVal
            let (highBool, highNum) = empty.Parse highVal

            let lowerErrMsg = "low must be less than or equal to high"
            let parseErrMsg = (sprintf "low and high must be %s values" empty.TypeName)
            
            match lowBool, highBool, lowNum <= highNum with
            | true, true, true -> Ok (lowNum, highNum)
            | true, true, false -> Error <| CSErrors (InvalidInitialState lowerErrMsg)
            | _ -> Error <| CSErrors (InvalidInitialState parseErrMsg)

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let deserialize colorString (numericCS: INumeric<'T>) =
        match (numericCS.Parse colorString, numericCS) with
        | Number i -> Color.pack i
        | OutOfRangeNumber -> Error <| CSErrors (OutOfRangeValue colorString)
        | NaN -> Error <| CSErrors (InvalidValue colorString)

    // /// Given a supposed member and a color set it checks if the value is a 
    // /// member of the set and return it's string color set value if it is.
    let serialize colorValue (numericCS : INumeric<'T>) =
        match ((true, Color.unpack colorValue), numericCS) with
        | Number _ -> Ok <| sprintf "%A" colorValue
        | OutOfRangeNumber -> Error <| CSErrors (OutOfRangeValue <| sprintf "%A" colorValue)
        | NaN -> Error <| CSErrors (InvalidColor <| sprintf "%A" colorValue)
    
    /// Given a value of the type it checks if it's a legal one
    let isLegal num (numericCS: INumeric<'T>) = 
        match ((true, num), numericCS) with 
        | Number _ -> true 
        | _ -> false

    /// Return a list of all posible values for this color set.
    let inline all (numericCS : INumeric<'T>) =
        match (numericCS.EmptyValue, numericCS) with
        | EmptyRange -> Error <| CSErrors (NotUsable "all")
        | Range (low, high) -> 
            Ok [] |> List.foldBack (fun colorRes accRes ->
                accRes >>= fun acc ->
                    colorRes >>= fun color ->
                        Ok <| color :: acc
                
            ) ([low..high] |> List.map Color.pack)

    /// Return the number of different vaules in this color set.
    let inline size (numericCS : INumeric<'T>) =
        match (numericCS.EmptyValue, numericCS) with
        | EmptyRange -> Error <| CSErrors (NotUsable "size")
        | Range (low, high) -> Ok <| int (high - low) + 1

    /// Return the actual value for the given position in this color set.
    let inline color index (numericCS : INumeric<'T>) =
        match (numericCS.EmptyValue, numericCS) with
        | EmptyRange -> Error <| CSErrors (NotUsable "color")
        | Range(low, high) when index >= low && index <= high -> Color.pack (index + low)
        | Range _ -> Error <| CSErrors (OutOfRangeIndex index)                 

    /// Return the ordinal position of every value in this color set.
    let inline ordinal colorValue (numericCS : INumeric<'T>) =
        let num = colorValue |> Color.unpack
        match (numericCS.EmptyValue, numericCS) with
        | EmptyRange -> Error <| CSErrors (NotUsable "ordinal")
        | Range(low, high) when num >= low && num <= high -> Ok (num - low)
        | Range _ -> Error <| CSErrors (OutOfRangeValue (string num))
                
