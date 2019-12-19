namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

type ColorSetId = CS of string

/// Type representing the posible values of every colorset (limited to simples
/// for now)
[<CustomEquality; CustomComparison>]
type Color =
    | Void
    | Unit of unit * ColorSetId
    | Bool of bool * ColorSetId
    | Int of int * ColorSetId
    | Bigint of bigint * ColorSetId
    | Float of float * ColorSetId
    | String of string * ColorSetId

    override xCSV.Equals(yObj) =
        match yObj with
        | :? Color as yCSV -> xCSV = yCSV
        | _ -> false

    override xCSV.GetHashCode() = hash xCSV

    interface System.IComparable with      
        member xCSV.CompareTo yObj =
            match yObj with
            | :? Color as yCSV -> 
                match xCSV, yCSV with
                | Void, Void -> 0
                | Unit (x, csx), Unit (y, csy) when csx = csy -> compare x y
                | Bool (x, csx), Bool (y, csy) when csx = csy -> compare x y
                | Int (x, csx), Int (y, csy) when csx = csy -> compare x y
                | Bigint (x, csx), Bigint (y, csy) when csx = csy -> compare x y
                | Float (x, csx), Float (y, csy) when csx = csy -> compare x y
                | String (x, csx), String (y, csy) when csx = csy -> compare x y
                | _ -> invalidArg "yObj" "cannot compare values of different ColorSets"
            | _ -> invalidArg "yObj" "cannot compare values outside of Color type"

module Color =
    let empty = Void

    let pack csid value =
        let csOrDefault = defaultArg csid

        match box value with
        | :? unit as unitColor -> Ok <| Unit (unitColor, csOrDefault <| CS "unit")
        | :? bool as boolColor -> Ok <| Bool (boolColor, csOrDefault <| CS "bool")
        | :? int as intColor -> Ok <| Int (intColor, csOrDefault <| CS "int")
        | :? bigint as bigintColor -> Ok <| Bigint (bigintColor, csOrDefault <| CS "bigint")
        | :? float as floatColor -> Ok <| Float (floatColor, csOrDefault <| CS "float")
        | :? string as stringColor -> Ok <| String (stringColor, csOrDefault <| CS "string")
        | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" value)
    
    let defaultPack value = pack None value   

    let unpack colorValue =
        match colorValue with
        | Void -> Error <| CSErrors (InvalidColor <| sprintf "void")
        | Unit (unitColor, csid) -> Ok (box unitColor, csid)
        | Bool (boolColor, csid) -> Ok (box boolColor, csid)
        | Int (intColor, csid) -> Ok (box intColor, csid)
        | Bigint (bigintColor, csid) -> Ok (box bigintColor, csid)
        | Float (floatColor, csid) -> Ok (box floatColor, csid)
        | String (stringColor, csid) -> Ok (box stringColor, csid)
    
    let colorValue = unpack >=> switch fst

    let colorSetId = unpack >=> switch snd

    let map (func : obj -> 'r) = 
        colorValue >=> switch func >=> defaultPack


