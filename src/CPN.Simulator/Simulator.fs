namespace CPN.Simulator

// Naive Implementation of the types involved in a petri Net; I'll be expanding
// each of this in an iterative manner and trying to implement the simulation
// for at least the Unit type without bindings

    type ColourSet = 
        | Unit of Unit

    type MultiSet = 
        { qty: int
          value: string
          colour: ColourSet }

    type Token = MultiSet list
    
    type Binding = string list

    type Place = 
        { colour: ColourSet 
          marking: Token list }

    type Transition = Binding list

    type ArcStructure = 
        | Input of Place * Transition
        | Output of Transition * Place

    type Expression = string

    type Arc = ArcStructure * Expression

    type CPN = Arc list


    module Simulator =

        let x = true
