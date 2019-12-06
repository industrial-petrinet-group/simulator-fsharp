namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

type VoidCS = 
    | VoidCS
    
    interface IColorSet<unit> with
        member _.MetaData = 
            { name = "VoidCS" 
              internalType = typeof<unit>
              colorSetHash = hash "" }
        
        member _.Init = fun () -> ()

        member _.Size = Ok 0
        
        member _.All = Ok []
    
        member _.Random = Error <| CSErrors (NotUsable "random")
    
        member _.ColorValue _colorString = Error <| CSErrors (NotUsable "colorValue")
    
        member _.IsLegal _colorValue = false
    
        member _.ColorString _colorValue = Error <| CSErrors (NotUsable "colorString")
    
        member _.Ordinal _colorValue = Error <| CSErrors (NotUsable "ordinal")
    
        member _.Color _index = Error <| CSErrors (NotUsable "color")

