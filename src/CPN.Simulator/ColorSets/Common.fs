namespace CPN.Simulator.ColorSets

module Common =
    type ColorSetErrors = 
        | NotUsable of func: string
        | InvalidInitialState of message: string
        | InvalidValue of value: string
        | OutOfRange of index: int

    let rnd = System.Random()




