namespace CPN.Simulator.Domain

/// Type representing the posible values of every colorset (limited to simples
/// for now)
type CSValue =
    | Unit of unit 
    | Bool of bool
    | Int of int
    | Bigint of bigint
    | Float of float
    | String of string

module CSValue =
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


