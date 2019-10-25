namespace CPN.Simulator.ColorSets

module Common =
    type ColorSetErrors = 
        | UnexcpectedValue of value: string
        | OutOfRange of index: int

    let rnd = System.Random()




