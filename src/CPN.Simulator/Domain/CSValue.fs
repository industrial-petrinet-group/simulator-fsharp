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
    let x = 2

