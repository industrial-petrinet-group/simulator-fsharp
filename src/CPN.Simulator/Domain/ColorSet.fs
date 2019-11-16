namespace CPN.Simulator.Domain

open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets

/// Type representing all posible Color Sets; it's restricted to Unit and 
/// Boolean for now.
type ColorSet = 
    | UnitCS of Unit
    | BooleanCS of Boolean
    
type ColorValue =
    | UnitVal of unit
    | BooleanVal of bool

/// Module implementing ColorSet's operations
module ColorSet =
    
    /// Given a Color Set it returns a random value from it; only work for 
    /// small Color Sets
    let randomVal = function
        | UnitCS unit -> 
            unit |> Unit.random >>= fun randomVal ->
                Unit.makeString randomVal unit
        | BooleanCS boolean -> 
            boolean |> Boolean.random >>= fun randomVal ->
                Boolean.makeString randomVal boolean
