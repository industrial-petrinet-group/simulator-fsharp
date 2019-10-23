namespace CPN.Simulator.ColorSets

open Common

type Boolean =
    { falsy: string
      truthy: string }

module Boolean =

    let create = function
        | None -> { falsy = "false"; truthy = "true" }
        | Some(falsyVal, truthyVal) -> { falsy = falsyVal; truthy = truthyVal }

    let isMember supposedMember booleanCS = 
        supposedMember = booleanCS.falsy || 
        supposedMember = booleanCS.truthy

    let defaultVal = false
    let all = [ false; true ]

    let size = 2
    let ordinal = function
        | false -> 0
        | true -> 1
    
    let colour = function
        | 0 -> Ok <| false
        | 1 -> Ok <| true
        | i -> Error <| OutOfRange i

    let random() = colour (rnd.Next(0,1))