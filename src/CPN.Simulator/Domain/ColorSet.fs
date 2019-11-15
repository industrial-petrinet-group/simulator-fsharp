namespace CPN.Simulator.Domain

open CPN.Simulator.Domain.ColorSets

/// Type representing all posible Color Sets; it's restricted to Unit for now.
type ColorSet = 
    | UnitCS of Unit
    | BooleanCS of Boolean