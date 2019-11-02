namespace CPN.Simulator

open CPN.Simulator.ColorSets
// Naive Implementation of the types involved in a petri Net; I'll be expanding
// each of this in an iterative manner and trying to implement the simulation
// for at least the Unit type without bindings

type ColorSet = 
    | UnitCS of Unit

type MultiSet = 
    { qty: int
      value: string
      colour: ColorSet }

type Marking = MultiSet list

type Binding = string list

type Place = 
    { name: string
      colour: ColorSet 
      marking: Marking }

type Transition =
    { name: string
      bindings: Binding list }

type ArcStructure = 
    | Input of Place * Transition
    | Output of Transition * Place

type Expression = string

type Arc = ArcStructure * Expression

type CPN = Arc list
/// Runtime module in charge of effectively do the simulation.
module Runtime =
    
    /// Definition of the most simple petri net
    let simpleNet =
        let (Ok unitCS) = Unit.create None

        let unitColour = UnitCS unitCS

        let unitToken = { qty = 1; value = "()"; colour = unitColour}
       
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

    /// parse a multi set color to string (this is needed because multiset are 
    /// not implemented yet)
    let parseMultiSet placeMarking = 
        "" |> List.foldBack (fun { qty = qty; value = value } acc ->
            match acc with
            | "" -> sprintf "%i`%s" qty value 
            | acc -> sprintf "%s++%i`%s" acc qty value 
        ) placeMarking

    /// return the state of the net; i.e the list of places with it's 
    /// respectives tokens for every place that have tokens
    let netMarking (net: CPN) =
        net
        |> List.map (function Input (p, _), _-> p | Output (_, p), _ -> p )
        |> List.filter (fun p -> p.marking <> [])

    /// return the triggered transitions
    let trigger (net: CPN) =
        net
        |> List.filter (function Input _, _-> true | _ -> false)
        |> List.fold (fun (acc: Map<Transition,Place list>) (Input (place, trans), _) ->
            match acc.TryFind trans with
            | None -> acc.Add (trans, [place])
            | Some placeList -> acc.Add (trans, place :: placeList)
        ) ([] |> Map.ofList)
        |> Map.filter (fun _ placeList -> 
            placeList |> List.forall (fun p -> p.marking <> []))
        


    
