namespace CPN.Simulator.Domain

open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets

/// Module implementing ColorSet's operations
module ColorSet =
    /// Return an empty colorset
    let empty = VoidCS :> IColorSet<unit>
    
    /// Given a supposed member it checks if is an actual member of the 
    /// colorset and return it's value if it is. 
    let inline colorValue colorString (cs: IColorSet<_>) = 
        cs.ColorValue colorString
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string color set value if it is. 
    let inline colorString colorValue (cs: IColorSet<_>) = 
        cs.ColorString colorValue
    
    /// Return a random value of this colorset.
    let inline randomValue (cs: IColorSet<_>) =
        cs.Random
    
    /// Return a random value of this colorset as a string.
    let inline randomString (cs: IColorSet<_>) =
        cs.Random
        >>= fun randomValue -> colorString randomValue cs
