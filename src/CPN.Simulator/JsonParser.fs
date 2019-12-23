namespace CPN.Simulator

open FSharp.Data

open CPN.Simulator.Domain
open CPN.Simulator.Operators

type CPNJson = JsonProvider<"./Data/sample-data.json">

/// Parser module for Json PetriNets
module JsonParser = 

    let parse (path : string option) =
        let data =
            match path with
            | Some path -> CPNJson.Load(new System.IO.StreamReader(path))
            | None -> CPNJson.GetSample()
        
        let placesRes =
            data.Places
            |> Array.map (fun place -> (place.Name, place.Color, place.Marking))
            |> Place.create

        placesRes
        >>= fun places ->
            data.Transitions
            |> Array.map (fun transition -> 
                transition.Name, transition.Guard, 
                transition.Inputs |> Array.foldBack (fun input acc -> 
                    (input.Place, input.Expression) :: acc) <| [], 
                transition.Outputs |> Array.foldBack (fun output acc -> 
                    (output.Place, output.Expression) :: acc) <| [])
            |> Transition.create places 
            >>= fun transitions ->
                Ok <| CPN (transitions, places)
            
                