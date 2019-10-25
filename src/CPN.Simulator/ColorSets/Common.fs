namespace CPN.Simulator.ColorSets

module Common =
    type ColorSetErrors = 
        | IlegalInitialState of message: string
        | IlegalValue of value: string
        | OutOfRange of index: int

    let rnd = System.Random()




