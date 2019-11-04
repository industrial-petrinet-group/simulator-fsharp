namespace CPN.Simulator

open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets

module SampleNets =
    // Simple definitions for convinience
    let (Ok unitCS) = Unit.create None
    let unitColour = UnitCS unitCS
    let unitToken = { qty = 1; value = "()"; colour = unitColour}

    /// Definition of the most simple petri net
    let simpleNet =
   
        let places = [|
            { name = "P1"; colour = unitColour; marking = [unitToken]}
            { name = "P2"; colour = unitColour; marking = []}
        |]

        let transition = { name = "T1"; bindings = [] }  

        let net : CPN = [
            Input (places.[0], transition), ""
            Output (transition, places.[1]), ""
        ]

        net
    
    /// Definition of an slightly more complex net
    let notSoSimpleNet = 
        
        let places = [|
            { name = "P1"; colour = unitColour; marking = [{unitToken with qty = 3}]}
            { name = "P2"; colour = unitColour; marking = [unitToken]}
            { name = "P3"; colour = unitColour; marking = []}
            { name = "P4"; colour = unitColour; marking = []}
            { name = "P5"; colour = unitColour; marking = []}
        |]

        let transitions = [|
            { name = "T1"; bindings = [] }
            { name = "T2"; bindings = [] }
        |]

        let net : CPN = [
            Input (places.[0], transitions.[0]), ""
            Input (places.[1], transitions.[0]), ""
            Output (transitions.[0], places.[1]), ""
            Output (transitions.[0], places.[2]), ""
            Input (places.[2], transitions.[1]), ""
            Output (transitions.[1], places.[3]), ""
            Output (transitions.[1], places.[4]), ""
        ]

        net