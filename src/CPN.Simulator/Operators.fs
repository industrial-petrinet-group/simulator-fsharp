namespace CPN.Simulator

open System.Text.RegularExpressions

/// Module for common operators inside the Simulator namespace
module Operators =
    let private random = System.Random()

    /// Return the identity inside a Result
    let rid x = Ok x

    /// Result bind operator
    let ( >>= ) m f = 
        match m with
        | Error err -> Error err
        | Ok x -> f x

    /// Result map function, converting a normal function to the result world
    let switch f x = Ok <| f x

    /// Get the keys of a Map
    let getKeys map =
        [] |> Map.foldBack (fun key _ acc -> key :: acc ) map

    /// Randomize a list order
    let randomizeList xs =
        xs |> List.sortBy (fun _ -> random.Next())    
    
    /// Active pattern for regexp matching    
    let (|Match|_|) pattern input =
        [for m in Regex.Matches(input, pattern) -> m]
        |> List.collect (fun m -> [ for g in m.Groups -> g.Value ] |> List.tail)
        |> function 
            | [] -> None
            | list -> Some list      
    