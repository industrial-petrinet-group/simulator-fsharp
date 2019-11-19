namespace CPN.Simulator.Domain

/// Type representing an Arc Id
type ArcId = A of int

/// Type representing a Arc Data
type ArcData = { expression: string }

/// Type representing a collection the Arcs
type Arcs = Arcs of Map<ArcId, ArcData>