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
type Transitions = Transitions of Map<TransitionId, TransitionData>

/// Module implementing Transition operations
module Transition =
    /// Given a serialized array of Transitions containing the name it
    /// generates the Transitions
    let create transitionSerializedList =
        transitionSerializedList
        |> Array.fold2 (fun acc id name ->
            acc |> Map.add (T id) { name = name; bindings = []}
        ) Map.empty [| 1..transitionSerializedList.Length |]
        |> Transitions