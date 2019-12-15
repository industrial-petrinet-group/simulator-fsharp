namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

[<StructuredFormatDisplay("VoidCS = {Show}")>]
type VoidCS = 
    | VoidCS
    
    interface ColorSet with
        member __.Name = "VoidCS"
        member __.Init = Unit ()
        member __.Serialize _colorString = Error <| CSErrors (NotUsable "colorValue")
        member __.Deserialize _colorValue = Error <| CSErrors (NotUsable "colorString")
        member __.IsLegal _colorValue = false
        member __.All = Ok []
        member __.Size = Ok 0
        member __.Ordinal _colorValue = Error <| CSErrors (NotUsable "ordinal")
        member __.Color _index = Error <| CSErrors (NotUsable "color")
        member __.Random = Error <| CSErrors (NotUsable "random")
    
    member this.Show = Common.asString (this :> ColorSet)

