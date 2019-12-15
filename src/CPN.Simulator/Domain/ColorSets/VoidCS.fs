namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

[<StructuredFormatDisplay("VoidCS = {Show}")>]
type VoidCS = 
    | VoidCS
    
    interface ColorSet with
        member _.Name = "VoidCS"
        member _.Init = Unit ()
        member _.Serialize _colorString = Error <| CSErrors (NotUsable "colorValue")
        member _.Deserialize _colorValue = Error <| CSErrors (NotUsable "colorString")
        member _.IsLegal _colorValue = false
        member _.All = Ok []
        member _.Size = Ok 0
        member _.Ordinal _colorValue = Error <| CSErrors (NotUsable "ordinal")
        member _.Color _index = Error <| CSErrors (NotUsable "color")
        member _.Random = Error <| CSErrors (NotUsable "random")
    
    member this.Show = Common.asString (this :> ColorSet)

