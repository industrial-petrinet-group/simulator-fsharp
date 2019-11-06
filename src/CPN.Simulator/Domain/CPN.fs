namespace CPN.Simulator.Domain

/// Type representing the Coloured Petri Net
type CPN = 
    | CPN of Places * Transitions * Arc list

/// Module for implementing all CPN's operations
module CPN = 
    /// Given a CPN it return the state of it; i.e the list of places with it's 
    /// respective tokens for every place that have at least one.
    let netMarking (CPN (places, transitions, arcs))  =
        arcs
        |> List.map (function Input (pid, _), _-> pid | Output (_, pid), _ -> pid )
        |> List.distinct
        |> Place.markingsAsStringList places
        |> List.filter (fun (_, marking) -> marking <> "")

    /// Given a CPN it return the transitions avaliable to be triggered.
    let toTrigger (CPN (places, transitions, arcs)) =  
        let filteredNet = 
            arcs |> List.filter (function Input _, _-> true | _ -> false)

        Map.empty
        |> List.foldBack (fun (Input (pid, tid), _) acc ->
            match acc.TryFind tid with
            | None -> acc.Add (tid, [pid])
            | Some placeList -> acc.Add (tid, pid :: placeList)
        ) filteredNet
        |> Map.filter (fun _ pids -> 
            pids 
            |> List.forall (fun pid -> 
                places 
                |> Map.tryFind pid 
                |> function 
                    | None -> false
                    | Some placeData -> placeData.marking <> []))
