namespace CPN.Simulator.Domain

open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets

/// Type representing all posible Color Sets values; it's restricted to Unit and 
/// Boolean for now.
type ColorSetVal = 
    | UnitVal of unit
    | BooleanVal of bool

/// Type representing all posible Color Sets; it's restricted to Unit and 
/// Boolean for now.
type ColorSet = 
    | UnitCS of Unit
    | BooleanCS of Boolean

/// Module implementing ColorSet's operations
module ColorSet =
    /// Given a supposed member and a Color Set it returns a string of the 
    /// value only if it actually was a member of the Color Set.
    let colorVal supposedMember = function
        | UnitCS unit -> 
            unit 
            |> Unit.colorVal supposedMember 
            >>= fun colorVal -> Ok <| UnitVal colorVal
        | BooleanCS boolean -> 
            boolean 
            |> Boolean.colorVal supposedMember 
            >>= fun colorVal -> Ok <| BooleanVal colorVal

    /// Given a Color Set it returns a random value from it; only work for 
    /// small Color Sets.
    let randomVal = function
        | UnitCS unit -> 
            unit 
            |> Unit.random 
            >>= fun randomVal -> Unit.makeString randomVal unit
        | BooleanCS boolean -> 
            boolean 
            |> Boolean.random 
            >>= fun randomVal -> Boolean.makeString randomVal boolean
