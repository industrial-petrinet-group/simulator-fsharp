namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

/// Type representing a Void ColorSet
type VoidCS = 
    | VoidCS
    
    interface IColorSet with
        member __.Name = "VoidCS"
        member __.Init = invalidOp "Void ColorSet doesn't have elements"
        member __.Serialize _colorString = Error <| CSErrors (NotUsable "serialize")
        member __.Deserialize _colorValue = Error <| CSErrors (NotUsable "deserialize")
        member __.IsLegal _colorValue = Error <| CSErrors (NotUsable "isLegal")
        member __.All = Error <| CSErrors (NotUsable "all")
        member __.Size = Error <| CSErrors (NotUsable "size")
        member __.Ordinal _colorValue = Error <| CSErrors (NotUsable "ordinal")
        member __.Color _index = Error <| CSErrors (NotUsable "color")
        member __.Random = Error <| CSErrors (NotUsable "random")

