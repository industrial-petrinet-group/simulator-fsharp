#load "./Operators.fs" 

#load "./Domain/ColorSets/Common.fs"
#load "./Domain/ColorSets/Unit.fs"
#load "./Domain/ColorSet.fs"
#load "./Domain/MultiSet.fs"
#load "./Domain/Place.fs"
#load "./Domain/Transition.fs"
#load "./Domain/Arc.fs"
#load "./Domain/Net.fs"
#load "./Domain/CPN.fs"


#load "./SampleNets.fs"
#load "./Runtime.fs" 


open CPN.Simulator
open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets;;


let (Ok unitCS) = Unit.create None
let unitColour = UnitCS unitCS
let unitToken = { qty = 1; value = "()"; colour = unitColour}

let net1 =
    let places = 
        Map.empty.
            Add(P 1, { name = "P1"; colour = unitColour; marking = [{unitToken with qty = 3}] }).
            Add(P 2, { name = "P2"; colour = unitColour; marking = [] }).
            Add(P 3, { name = "P3"; colour = unitColour; marking = [unitToken] }).
            Add(P 4, { name = "P4"; colour = unitColour; marking = [] }).
            Add(P 5, { name = "P5"; colour = unitColour; marking = [] })

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
            Add(T 1, 
                    { i = [(P 1, A 1); (P 2, A 2)]
                      o = [(P 2, A 3); (P 3, A 4)]}).
            Add(T 2, 
                    { i = [(P 3, A 5)]
                      o = [(P 4, A 5); (P 5, A 7)]})
    

    CPN (net, (places, transitions, arcs))

let net2 =
    let places = 
        Map.empty.
            Add(P 1, { name = "P1"; colour = unitColour; marking = [{unitToken with qty = 3}] }).
            Add(P 2, { name = "P2"; colour = unitColour; marking = [unitToken] }).
            Add(P 3, { name = "P3"; colour = unitColour; marking = [unitToken] }).
            Add(P 4, { name = "P4"; colour = unitColour; marking = [] }).
            Add(P 5, { name = "P5"; colour = unitColour; marking = [] })

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
            Add(T 1, 
                    { i = [(P 1, A 1); (P 2, A 2)]
                      o = [(P 2, A 3); (P 3, A 4)]}).
            Add(T 2, 
                    { i = [(P 3, A 5)]
                      o = [(P 4, A 5); (P 5, A 7)]})
    

    CPN (net, (places, transitions, arcs))

let a = SampleNets.notSoSimpleNet |> Runtime.step
let b = net1 |> Runtime.step
let c = net2 |> Runtime.step;;