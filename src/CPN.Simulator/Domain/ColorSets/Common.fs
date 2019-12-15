namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

/// Module implementing common functions for ColorSets
module Common =
    /// Random generator 
    let rnd = System.Random()
    
    /// Given a IColorSet it returns a String representing it.
    let asString (cs: ColorSet) =
        match cs.All with
        | Error _ -> ""
        | Ok list -> sprintf "%A" list
        |> sprintf "%s"
   

