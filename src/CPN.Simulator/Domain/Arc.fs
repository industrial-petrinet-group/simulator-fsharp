namespace CPN.Simulator.Domain

/// Type representing the structure of the two kinds of Arcs
type ArcStructure = 
    | Input of PlaceId * TransitionId
    | Output of TransitionId * PlaceId

/// Type repressenting a Expressión associated to an Arc
type Expression = string

/// Type representing the Arc
type Arc = ArcStructure * Expression