namespace CPN.Simulator

open System.Text.RegularExpressions

/// Module for common operators inside the Simulator namespace
module Operators =
    let random = System.Random()

    /// Return the identity inside a Result
    let rid x = Ok x

    /// Result bind operator
    let ( >>= ) m f = 
        match m with
        | Error err -> Error err
        | Ok x -> f x

    /// Result return function, converting a normal function to the result world
    let switch f x = Ok <| f x

    /// Result monadic composition function
    let (>=>) switch1 switch2 x = 
        match switch1 x with
        | Ok s -> switch2 s
        | Error e -> Error e 

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
    
    /// Compare 2 maps expecting the first to be less thanm the second and 
    // returns 1, 0 and -1 as compare function or an exception if they are 
    // disjoint key-wise.
    // FIXME: This is really complex, it should be refactored.
    let compareMap disjointMsg compareCount lessMap moreMap =
        let disjointExn () = invalidArg "yObj" disjointMsg
        let lessKeys, moreKeys = lessMap |> getKeys, moreMap |> getKeys

        match lessKeys = moreKeys, compareCount with
        | true, 0 ->
            lessKeys |> List.fold (fun (mantained, lastComp) key ->
                match mantained with
                | false -> false, 0
                | true -> 
                    let newComp = compare lessMap.[key] moreMap.[key]
                    lastComp = newComp || newComp = 0, 
                    if (newComp = 0) then lastComp else newComp
            ) (true, (compare lessMap.[List.head lessKeys] moreMap.[List.head lessKeys]))
            |> function
                | true, compared -> compared
                | false, _ -> disjointExn ()
        | false, _ ->
            moreKeys 
            |> List.fold (fun acc key ->
                match acc with
                | [] -> acc
                | head :: tail -> if head = key then tail else head::tail 
            ) lessKeys
            |> function
                | [] -> 
                    lessKeys 
                    |> List.forall (fun key -> lessMap.[key] <= moreMap.[key])
                    |> function
                        | true -> compareCount
                        | false -> disjointExn ()
                | _ -> disjointExn ()

        | true, _ -> disjointExn ()   
    