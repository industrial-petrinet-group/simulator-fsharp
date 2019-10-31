namespace CPN.Simulator

    type ColourSet = 
        | Unit

    type MultiSet = 
        { qty: int
          value: string
          colour: ColourSet }

    type Token = MultiSet list
    
    type Binding = string list

    type Place = 
        { colour: ColourSet 
          tokens: Token list }

    type Transition = Binding list

    type ArcStructure = 
        | Input of Place * Transition
        | Output of Transition * Place

    type Expression = string

    type Arc = ArcStructure * Expression

    type CPN = Arc list
