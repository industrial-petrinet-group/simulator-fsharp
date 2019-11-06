namespace CPN.Simulator

open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets

module SampleNets =
    // Simple definitions for convinience
    let (Ok unitCS) = Unit.create None
    let unitColour = UnitCS unitCS
    let unitToken = { qty = 1; value = "()"; colour = unitColour}

    /// Definition of the most simple petri net
    let simpleNet : CPN =
   
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; colour = unitColour; marking = [unitToken] }).
                Add(P 2, { name = "P2"; colour = unitColour; marking = [] })

        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; bindings = [] })

        let arcs = [
            Input (P 1, T 1), ""
            Output (T 1, P 2), ""
        ]
        
        CPN (places, transitions, arcs)
    
    /// Definition of an slightly more complex net
    let notSoSimpleNet : CPN = 
        
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; colour = unitColour; marking = [{unitToken with qty = 3}] }).
                Add(P 2, { name = "P2"; colour = unitColour; marking = [unitToken] }).
                Add(P 3, { name = "P3"; colour = unitColour; marking = [] }).
                Add(P 4, { name = "P4"; colour = unitColour; marking = [] }).
                Add(P 5, { name = "P5"; colour = unitColour; marking = [] })

        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; bindings = [] }).
                Add(T 2, { name = "T2"; bindings = [] })

        let arcs = [
            Input (P 1, T 1), ""
            Input (P 2, T 1), ""
            Output (T 1, P 2), ""
            Output (T 1, P 3), ""
            Input (P 3, T 2), ""
            Output (T 2, P 4), ""
            Output (T 2, P 5), ""
        ]

        CPN (places, transitions, arcs)