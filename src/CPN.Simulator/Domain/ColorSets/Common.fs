namespace CPN.Simulator.Domain.ColorSets

module Common =
    type ColorSetErrors = 
        | NotUsable of func: string
        | InvalidInitialState of message: string
        | InvalidValue of value: string
        | OutOfRangeValue of value: string
        | OutOfRangeIndex of index: obj

    let rnd = System.Random()
   

