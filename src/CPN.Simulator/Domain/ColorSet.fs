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
[<StructuredFormatDisplay("{Show}")>]
type ColorSet =
    | VoidCS
    | UnitCS of Unit
    | BooleanCS of Boolean

    /// Reimplements the way of showing the CPN
    member this.Show = 
        match this with
        | VoidCS -> "Void Color Set"
        | UnitCS unit -> Unit.asString unit
        | BooleanCS boolean -> Boolean.asString boolean    

/// Module implementing ColorSet's operations
module ColorSet =
    /// Return an empty Color Set
    let empty = VoidCS

    /// Given a supposed member and a Color Set it returns a string of the 
    /// value only if it actually was a member of the Color Set.
    let colorVal supposedMember = function
        | VoidCS ->
            Error <| CSErrors (NotUsable "colorVal")
        | UnitCS unit -> 
            unit 
            |> Unit.colorVal supposedMember 
            >>= fun colorVal -> Ok <| UnitVal colorVal
        | BooleanCS boolean -> 
            boolean 
            |> Boolean.colorVal supposedMember 
            >>= fun colorVal -> Ok <| BooleanVal colorVal
    
    let makeString colorVal colorSet =
        match colorVal, colorSet with
        | UnitVal unitVal, UnitCS unit -> Unit.makeString unitVal unit
        | BooleanVal boolVal, BooleanCS bool -> Boolean.makeString boolVal bool

    /// Given a Color Set it returns a random value from it; only work for 
    /// small Color Sets.
    let randomVal = function
        | VoidCS ->
            Error <| CSErrors (NotUsable "randomVal")
        | UnitCS unit -> 
            unit 
            |> Unit.random 
            >>= fun randomVal -> Unit.makeString randomVal unit
        | BooleanCS boolean -> 
            boolean 
            |> Boolean.random 
            >>= fun randomVal -> Boolean.makeString randomVal boolean
