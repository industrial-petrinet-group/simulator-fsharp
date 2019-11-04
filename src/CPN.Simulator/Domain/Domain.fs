namespace CPN.Simulator.Domain
// Naive Implementation of the types involved in a petri Net; I'll be expanding
// each of this in an iterative manner and trying to implement the simulation
// for at least the Unit type without bindings

type Marking = MultiSet list

type Binding = string list

type Place = 
    { name: string
      colour: ColorSet 
      marking: Marking }

type Transition =
    { name: string
      bindings: Binding list }

type ArcStructure = 
    | Input of Place * Transition
    | Output of Transition * Place

type Expression = string

type Arc = ArcStructure * Expression

type CPN = Arc list