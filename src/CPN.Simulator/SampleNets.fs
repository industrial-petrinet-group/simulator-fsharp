namespace CPN.Simulator

open CPN.Simulator.Domain

// Should make creation API's
module SampleNets =
    // Simple definitions for convinience
    let (Ok unitColor) = ColorSet.ofColor ()
    let (Ok boolColor) = ColorSet.ofColor true
    let emptyMS = MultiSet.emptyWithColor unitColor 
    let emptyBMS = MultiSet.emptyWithColor boolColor
    let (Ok unitMS) = MultiSet.emptyWithColor unitColor |> MultiSet.addTokens 1 
    let (Ok unitMS3) = unitMS |> MultiSet.addTokens 2
    let (Ok boolMS) = MultiSet.emptyWithColor boolColor |> MultiSet.addTokens 1 

    /// Definition of the most simple petri net
    let simpleNet : CPN =
   
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; marking = unitMS }).
                Add(P 2, { name = "P2"; marking = emptyMS })

        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; bindings = [] })

        let arcs = 
            Map.empty.
                Add(A 1, {expression = ""}).
                Add(A 2, {expression = ""})
        
        let net = 
            Map.empty.
                Add(T 1, {i = [(P 1, A 1)]; o = [(P 2, A 2)]})
        
        CPN (Net net, (Places places, Transitions transitions, Arcs arcs))
    
    /// Definition of the most simple booleanpetri net
    let simpleBooleanNet : CPN =
   
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; marking = boolMS }).
                Add(P 2, { name = "P2"; marking = emptyMS })

        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; bindings = [] })

        let arcs = 
            Map.empty.
                Add(A 1, {expression = ""}).
                Add(A 2, {expression = ""})
        
        let net = 
            Map.empty.
                Add(T 1, {i = [(P 1, A 1)]; o = [(P 2, A 2)]})
        
        CPN (Net net, (Places places, Transitions transitions, Arcs arcs))
    /// Definition of an slightly more complex net
    let notSoSimpleNet : CPN = 
        
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; marking = unitMS3 }).
                Add(P 2, { name = "P2"; marking = unitMS }).
                Add(P 3, { name = "P3"; marking = emptyMS }).
                Add(P 4, { name = "P4"; marking = emptyMS }).
                Add(P 5, { name = "P5"; marking = emptyMS })

        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; bindings = [] }).
                Add(T 2, { name = "T2"; bindings = [] })

        let arcs = 
            Map.empty.
                Add(A 1, {expression = ""}).
                Add(A 2, {expression = ""}).
                Add(A 3, {expression = ""}).
                Add(A 4, {expression = ""}).
                Add(A 5, {expression = ""}).
                Add(A 6, {expression = ""}).
                Add(A 7, {expression = ""})
        
        let net =
            Map.empty.
                Add(T 1, { i = [(P 1, A 1); (P 2, A 2)]
                           o = [(P 2, A 3); (P 3, A 4)]}).
                           
                Add(T 2, { i = [(P 3, A 5)]
                           o = [(P 4, A 6); (P 5, A 7)]})
        

        CPN (Net net, (Places places, Transitions transitions, Arcs arcs))

    let randomlyPathedNet =
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; marking = unitMS }).
                Add(P 2, { name = "P2"; marking = unitMS }).
                Add(P 3, { name = "P3"; marking = unitMS }).
                Add(P 4, { name = "P4"; marking = emptyMS }).
                Add(P 5, { name = "P5"; marking = emptyMS })

        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; bindings = [] }).
                Add(T 2, { name = "T2"; bindings = [] })

        let arcs = 
            Map.empty.
                Add(A 1, {expression = ""}).
                Add(A 2, {expression = ""}).
                Add(A 3, {expression = ""}).
                Add(A 4, {expression = ""}).
                Add(A 5, {expression = ""}).
                Add(A 6, {expression = ""}).
                Add(A 7, {expression = ""})
        
        let net =
            Map.empty.
                Add(T 1, { i = [(P 1, A 1); (P 2, A 2)]
                           o = [(P 4, A 3)]}).
                           
                Add(T 2, { i = [(P 2, A 4); (P 3, A 5)]
                           o = [(P 5, A 6)]})
        

        CPN (Net net, (Places places, Transitions transitions, Arcs arcs))