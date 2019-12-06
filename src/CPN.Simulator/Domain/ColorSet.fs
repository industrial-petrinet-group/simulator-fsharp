namespace CPN.Simulator.Domain

open CPN.Simulator.Operators
open CPN.Simulator.Domain.ColorSets

type ColorSet =
    | Void of VoidCS
    | Unit of UnitCS
    | Boolean of UnitCS

// Look for a way to compare colors in multiset and have the ability to check
// for the internal type. Also think if it's plausible to use this way of 
// constraining through members or should be better to use an Interface that will
// provide the abobe kind of comparission for free without the choice type.

/// Module implementing ColorSet's operations
module ColorSet =
    /// Return an empty Color Set
    let empty = Void <| VoidCS

    let inline colorValue colorString cs = 
        (^T: (member ColorValue:_->_) (cs, colorString))
    
    let inline colorString colorValue cs = 
        (^T: (member ColorString:_->_) (cs, colorValue))

    let inline randomValue cs =
        (^T: (member Random: _) cs)
    
    let inline randomString cs =
        (^T: (member Random: Result<_,_>) cs) 
        >>= fun randomValue -> colorString randomValue cs
