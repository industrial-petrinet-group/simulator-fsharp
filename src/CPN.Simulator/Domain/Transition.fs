namespace CPN.Simulator.Domain

/// Type representing the bindings of a transition
type Binding = string list

/// Type representing a Transition Id
type TransitionId = T of int

/// Type representing a Transition Data
type TransitionData =
    { name: string
      bindings: Binding list }

/// Type representing a collection of transitions; check if a Dictionary is
/// needed due to the dinamic nature of bindings and the opposed inmutable maps.
type Transitions = Map<TransitionId, TransitionData>
