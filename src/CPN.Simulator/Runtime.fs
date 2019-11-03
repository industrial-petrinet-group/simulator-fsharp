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
        |> List.distinct

    /// return the triggered transitions
    let trigger (net: CPN) =       
        let emptyMap: Map<Transition,Place list> = ([] |> Map.ofList)

        let filteredNet = 
            net |> List.filter (function Input _, _-> true | _ -> false)

        emptyMap
        |> List.foldBack (fun (Input (place, trans), _) acc ->
            match acc.TryFind trans with
            | None -> acc.Add (trans, [place])
            | Some placeList -> acc.Add (trans, place :: placeList)
        ) filteredNet
        |> Map.filter (fun _ placeList -> 
            placeList |> List.forall (fun p -> p.marking <> []))

    /// remove the input tokens from the places involved in trigggering the 
    /// transition.
    let removeInputTokens (toTrigger: Map<Transition, Place list>) net : CPN =
        net 
        |> List.map (function
            | Input (p, t), e when toTrigger.ContainsKey t ->
                let newMarking = 
                    p.marking
                    |> List.head // Given it's only one type there is no need for more logic now
                    |> function
                        | {qty = 1} -> []
                        | token -> [{token with qty = token.qty - 1}]

                Input ({p with marking = newMarking} , t), e    
            | otherwise -> otherwise)
    
    /// add the output tokens for the places reached by the triggered transition.
    let addOutputTokens (toTrigger: Map<Transition, Place list>) net : CPN =
        net 
        |> List.map (function
            | Output (t, p), e when toTrigger.ContainsKey t ->
                let newMarking = 
                    p.marking
                    |> function
                        | [] -> [unitToken]
                        | token::rest -> {token with qty = token.qty + 1} :: rest
                        // as in it's counterpart; it's simple beacause there is only 1 token

                Output (t, {p with marking = newMarking}), e    
            | otherwise -> otherwise)

    /// make a step in the net
    let step (net: CPN) =
        let toTrigger = trigger net

        match Map.isEmpty toTrigger with
        | true -> false, net
        | false ->
            net 
            |> removeInputTokens toTrigger 
            |> addOutputTokens toTrigger
            |> fun modifiedNet -> true, modifiedNet

        


    
