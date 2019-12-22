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
        
        let places =
            data.Places
            |> Array.map (fun place -> (place.Name, place.Color, place.Marking))
            |> Place.create

        //let transitions =
        //    data.Net
        //    |> Array.map (fun net -> net.Transition, net.Guard)
        //    |> Transition.create
        
        //// Continue from this!
        //let net =
        //    data.Net
        //    |> Array.map (fun transitionIO -> 
        //        transitionIO.Transition, transitionIO.Input, transitionIO.Output)
        //    |> Net.create (transitions, places, arcs)


        2
            
                