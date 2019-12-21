namespace CPN.Simulator.Domain

/// Type representing the posible values of every colorset (limited to simple)
[<StructuredFormatDisplay("{Show}")>]
[<CustomEquality; CustomComparison>]
type Color =
    | Unit
    | Bool of bool
    | Int of int
    | Bigint of bigint
    | Float of float
    | String of string

    member this.Unpack () =
        match this with
        | Unit -> () |> box |> unbox
        | Bool boolColor -> boolColor |> box |> unbox
        | Int intColor -> intColor |> box |> unbox
        | Bigint bigintColor -> bigintColor |> box |> unbox
        | Float floatColor -> floatColor |> box |> unbox
        | String stringColor -> stringColor |> box |> unbox

    override xCSV.Equals(yObj) =
        match yObj with
        | :? Color as yCSV -> xCSV.GetHashCode() = yCSV.GetHashCode()
        | _ -> false

    override xCSV.GetHashCode() = hash <| xCSV.Unpack()

    interface System.IComparable with      
        member xCSV.CompareTo yObj =
            match yObj with
            | :? Color as yCSV -> 
                match xCSV, yCSV with
                | Unit, Unit -> 0
                | Bool x, Bool y -> compare x y
                | Int x, Int y -> compare x y
                | Bigint x, Bigint y -> compare x y
                | Float x, Float y -> compare x y
                | String x, String y -> compare x y
                | _ -> invalidArg "yObj" "cannot compare values of different ColorSets"
            | _ -> invalidArg "yObj" "cannot compare values outside of Color type"


/// Module implementing Color related operations
module Color =
    /// Return the empty Color
   // let empty = Void

    /// Given a value it tries to pack them inside a Color
    let pack value =
        match box value with
        | :? unit -> Ok <| Unit
        | :? bool as boolColor -> Ok <| Bool boolColor
        | :? int as intColor -> Ok <| Int intColor
        | :? bigint as bigintColor -> Ok <| Bigint bigintColor
        | :? float as floatColor -> Ok <| Float floatColor
        | :? string as stringColor -> Ok <| String stringColor
        | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" value)

    /// Given a Color it unpacks it
    let unpack (color : Color) = color.Unpack ()

    /// Given a mapping function it maps a Color to a new one based on the 
    /// defaults ColorSetIds 
    let map mapping = 
        unpack >> mapping >> pack

type Color with
    member this.Show =
        match this with
        | Unit -> sprintf "()"
        | Bool boolColor -> sprintf "%b" boolColor
        | Int intColor -> sprintf "%i" intColor
        | Bigint bigintColor -> sprintf "%O" bigintColor
        | Float floatColor -> sprintf "%F" floatColor
        | String stringColor -> stringColor