namespace CPN.Simulator

/// Module for common operators inside the Simulator namespace
module Operators =

    /// Return the identity inside a Result
    let rid x = Ok x

    /// Result bind operator
    let ( >>= ) m f = 
        match m with
        | Error err -> Error err
        | Ok x -> f x

    /// Result map function, converting a normal function to the result world
    let switch f x = Ok <| f x

    