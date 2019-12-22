namespace CPN.Simulator.Domain

/// Type representing an Arc Id
type ArcId = A of int

/// Type representing a Arc Data
type ArcData = { expression: string }

/// Type representing a collection the Arcs
type Arcs = Arcs of Map<ArcId, ArcData>

/// Module implementing Arc operations
module Arc =
    /// Given a serialized array of Arcs it generates the Arcs
    let create arcsSerializedList =
        arcsSerializedList
        |> Array.fold2 (fun acc id _name ->
            acc |> Map.add (A id) { expression = "" }
        ) Map.empty [| 1..arcsSerializedList.Length |]
        |> Arcs