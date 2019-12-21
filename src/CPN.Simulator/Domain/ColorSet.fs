namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Module implementing ColorSet's operations
module ColorSet =
    /// Module for private implementation details
    [<AutoOpen>]
    module private Implementation =
        let defaultColorSet color =
            match color with
            | Unit _ -> Ok <| CS "unit"
            | Bool _ -> Ok <| CS "bool"
            | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" color)

        let declaredColorSetAction csid callback =
             csid
             |> Declaration.colorSet (Declaration.actuals())
             >>= callback

    let ofColor color = color |> Color.pack >>= defaultColorSet

    /// Return the empty ColorSetId
    let empty = CS "void"
    
    /// Given a coloret id it returns the initial value for the colorset
    let init (csid: ColorSetId) =
        declaredColorSetAction csid <| fun cs -> rid cs.Init

    /// Given a supposed member it checks if is an actual member of the 
    /// colorset and return it's value if it is. 
    let deserialize (csid: ColorSetId) supossedMember = 
        declaredColorSetAction csid <| fun cs -> cs.Deserialize supossedMember
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string color set value if it is. 
    let serialize (csid: ColorSetId) color = 
        declaredColorSetAction csid <| fun cs -> cs.Serialize color
    
    /// Given a colorsetid and a color it checks if the later is a legal one.
    let isLegal (csid: ColorSetId) color =
        declaredColorSetAction csid <| fun cs -> cs.IsLegal color

    /// Return a random value of this colorset.
    let random (csid: ColorSetId) = 
        declaredColorSetAction csid <| fun cs -> cs.Random
    
    /// Return a random value of this colorset as a string.
    let randomAsString (csid: ColorSetId) = 
        declaredColorSetAction csid <| fun cs -> cs.Random >>= (serialize csid)

   