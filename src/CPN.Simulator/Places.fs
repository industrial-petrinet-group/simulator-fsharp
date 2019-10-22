namespace CPN.Simulator

type ColorSet =
    | Unit of unit: string
    | Boolean of falsy: string * truthy: string


    static member IsMember supposedMember =
        function
        | Unit unitValue -> supposedMember = unitValue
        | Boolean(falsy, truthy) -> supposedMember = falsy || supposedMember = truthy

module Definitions =
    let defineUnit _timed =
        function
        | None -> Unit "()"
        | Some unitValue -> Unit unitValue

    let defineBoolean _timed =
        function
        | None -> Boolean("false", "true")
        | Some(falsy, truthy) -> Boolean(falsy, truthy)


type Token = int * ColorSet

type Place =
    { name: string
      colorSet: ColorSet
      token: Token list }
