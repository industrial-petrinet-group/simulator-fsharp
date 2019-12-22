namespace CPN.Simulator

open CPN.Simulator.Domain

// Should make creation API's
module SampleNets =
    // Simple definitions for convinience
    let (Ok unitColor) = ColorSet.ofColorValue ()
    let (Ok boolColor) = ColorSet.ofColorValue true
    let emptyMS = MultiSet.emptyWithColor unitColor 
    let emptyBMS = MultiSet.emptyWithColor boolColor
    let (Ok unitMS) = MultiSet.emptyWithColor unitColor |> MultiSet.addTokens 1 
    let (Ok unitMS3) = unitMS |> MultiSet.addTokens 2
    let (Ok boolMS) = MultiSet.emptyWithColor boolColor |> MultiSet.addTokens 1 

    let emptyExpr = E ""

    /// Definition of the most simple petri net
    let simpleNet : CPN =
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; marking = unitMS }).
                Add(P 2, { name = "P2"; marking = emptyMS })
        
        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; guard = emptyExpr 
                           inputs = [(P 1, emptyExpr)]; outputs = [(P 2, emptyExpr)]})
        
        CPN (Transitions transitions, Places places)
    
    /// Definition of the most simple booleanpetri net
    let simpleBooleanNet : CPN =
   
        let places = 
            Map.empty.
                Add(P 1, { name = "P1"; marking = boolMS }).
                Add(P 2, { name = "P2"; marking = emptyMS })
        
        let transitions = 
            Map.empty.
                Add(T 1, { name = "T1"; guard = emptyExpr 
                           inputs = [ (P 1, emptyExpr) ]
                           outputs = [ (P 2, emptyExpr) ] })
        
        CPN (Transitions transitions, Places places)
    
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
                Add(T 1, { name = "T1"; guard = emptyExpr
                           inputs = [(P 1, emptyExpr); (P 2, emptyExpr)]
                           outputs = [(P 2, emptyExpr); (P 3, emptyExpr)] }).
                           
                Add(T 2, { name = "T2"; guard = emptyExpr
                           inputs = [(P 3, emptyExpr)]
                           outputs = [(P 4, emptyExpr); (P 5, emptyExpr)]})
        

        CPN (Transitions transitions, Places places)

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
                Add(T 1, { name = "T1"; guard = emptyExpr
                           inputs = [(P 1, emptyExpr); (P 2, emptyExpr)]
                           outputs = [(P 4, emptyExpr)]}).
                           
                Add(T 2, { name = "T2"; guard = emptyExpr
                           inputs = [(P 2, emptyExpr); (P 3, emptyExpr)]
                           outputs = [(P 5, emptyExpr)]})
        

        CPN (Transitions transitions, Places places)