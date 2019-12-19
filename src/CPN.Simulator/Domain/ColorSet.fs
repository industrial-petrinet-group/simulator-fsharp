namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Module implementing ColorSet's operations
module ColorSet =
    /// Module for private implementation details
    [<AutoOpen>]
    module private Implementation =
        let toIColorSet cs = (cs :> IColorSet)

    let ofColor color =
        color
        |> Color.defaultPack
        >>= Color.colorSetId
    
    /// Return an empty colorset
    let empty = CS "void"
    
    /// Given a supposed member it checks if is an actual member of the 
    /// colorset and return it's value if it is. 
    let deserialize (csid: ColorSetId) = 
        csid
        |> Declarations.colorSet (Declarations.defaults)
        |> function Some cs -> cs.Deserialize | None -> Error <|
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string color set value if it is. 
    let serialize (cs : IColorSet) = cs.Serialize 
    
    /// Return a random value of this colorset.
    let random (cs: IColorSet) = cs.Random
    
    /// Return a random value of this colorset as a string.
    let inline randomAsString (cs: IColorSet) = cs.Random >>= (serialize cs)

   