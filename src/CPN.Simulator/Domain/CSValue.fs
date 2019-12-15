namespace CPN.Simulator.Domain

/// Type representing the posible values of every colorset (limited to simples
/// for now)
[<CustomEquality; CustomComparison>]
type CSValue =
    | Void
    | Unit of unit 
    | Bool of bool
    | Int of int
    | Bigint of bigint
    | Float of float
    | String of string

    override xCSV.Equals(yObj) =
        match yObj with
        | :? CSValue as yCSV -> xCSV = yCSV
        | _ -> false

    override xCSV.GetHashCode() = hash (xCSV)

    interface System.IComparable with      
        member xCSV.CompareTo yObj =
            match yObj with
            | :? CSValue as yCSV -> 
                match xCSV, yCSV with
                | Void, Void -> 0
                | Unit x, Unit y -> compare x y
                | Bool x, Bool y -> compare x y
                | Int x, Int y -> compare x y
                | Bigint x, Bigint y -> compare x y
                | Float x, Float y -> compare x y
                | String x, String y -> compare x y
                | _ -> invalidArg "yObj" "cannot compare values of different CSValue types"
            | _ -> invalidArg "yObj" "cannot compare values outside of CSValue type"

module CSValue =
    let empty = Void

    let pack color =
        match box color with
        | :? unit as unitColor -> Ok <| Unit unitColor
        | :? bool as boolColor -> Ok <| Bool boolColor
        | :? int as intColor -> Ok <| Int intColor
        | :? bigint as bigintColor -> Ok <| Bigint bigintColor
        | :? float as floatColor -> Ok <| Float floatColor
        | :? string as stringColor -> Ok <| String stringColor
        | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" color)
    
    let unpack colorValue =
        match colorValue with
        | Unit unitColor -> box unitColor
        | Bool boolColor -> box boolColor
        | Int intColor -> box intColor
        | Bigint bigintColor -> box bigintColor
        | Float floatColor -> box floatColor
        | String stringColor -> box stringColor
    
    let map (func : obj -> 'r) = unpack >> func >> pack


