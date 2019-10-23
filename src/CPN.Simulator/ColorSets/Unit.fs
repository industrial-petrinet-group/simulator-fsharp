namespace CPN.Simulator.ColorSets

open Common

type Unit =
    { unit: string }

module Unit =

    let create =
        function
        | None -> { unit = "()" }
        | Some unitVal -> { unit = unitVal }

    let isMember supposedMember unitCS = (unitCS.unit = supposedMember)

    let defaultVal = ()
    let all = [ () ]

    let size = 1
    let ordinal() = 0

    let colour =
        function
        | 0 -> Ok <| ()
        | i -> Error <| OutOfRange i

    let random() = ()
