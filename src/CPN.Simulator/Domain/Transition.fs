namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Type representing the ID of a transitions
type TransitionId = T of int

/// Type representing Expressions to be evaluated
type Expression = E of string

/// Type representing the the list of inputs and outputs of a transition.
type TransitionData = 
    { name: string
      guard: Expression
      inputs: (PlaceId * Expression) list
      outputs: (PlaceId * Expression) list }

/// Type representing the whole Net; it's core is based upon transitions because
/// it's the center of the simulation.
[<StructuredFormatDisplay("Transitions = {Show}")>]
type Transitions = Transitions of Map<TransitionId, TransitionData>

/// Module implementing Net's operations.
module Transition =
    /// Create the Transtions
    let create places serializedTransitionsArray =
        let foldParserIO (placeName, expression) accRes =
            accRes >>= fun acc ->
                match places |> Place.idFromName placeName with
                | None -> Error <| PAErrors TransitionsParsingError
                | Some pid -> Ok <| (pid, E expression) :: acc

        serializedTransitionsArray
        |> Array.fold2 (fun accRes id (name, guard, inputs, outputs) ->
            let parsedInputsRes = Ok [] |> List.foldBack foldParserIO inputs
            let parsedOutputsRes = Ok [] |> List.foldBack foldParserIO outputs
            
            accRes >>= fun acc -> 
                parsedInputsRes >>= fun parsedInputs ->
                    parsedOutputsRes >>= fun parsedOutputs ->
                        Ok (acc |> Map.add (T id) { name = name
                                                    guard = E guard 
                                                    inputs = parsedInputs
                                                    outputs = parsedOutputs })

        ) (Ok Map.empty) [| 1..serializedTransitionsArray.Length |]
        >>= fun transitions -> Ok <| Transitions transitions

    /// Returns and empty Transition
    let empty = Transitions Map.empty

    /// Given Transitions check if they're empty
    let isEmpty (Transitions transitions) = transitions |> Map.isEmpty
    
    /// Given Transitions it returns a list of Transitions ID
    let keyList (Transitions transitions) = 
        [] |> Map.foldBack (fun key _ acc -> key :: acc ) transitions

    /// Given Transitions it returns a random ordered list of Transitions ID to 
    /// retrive the information of the Net randomly
    let randomKeyList (Transitions transitions) = 
        transitions
        |> Map.fold (fun acc key _ -> key :: acc ) []
        |> randomizeList
    
    /// Given a TransitionId and Transitions it returns the corresponding 
    /// TransitionData
    let find tid (Transitions transitions) = transitions |> Map.find tid

    /// Given a Transitions, add a pair of TransitionId and TransitionData.
    let add tid transitionIO (Transitions transitions) = 
        Transitions <| transitions.Add(tid, transitionIO)

    /// Given a TransitionId and Transitions it returns a PlaceId list of it's 
    /// inputs.
    let inputs tid transitions =
        let { inputs = inputs } = transitions |> find tid

        inputs |> List.map fst
    
    /// Given a TransitionId and Transitions it returns a PlaceId list of it's 
    /// outputs.
    let outputs tid transitions =
        let { outputs = outputs } = transitions |> find tid

        outputs |> List.map fst

    /// Given Places, a transition Id and Transitions it returns if the tid is 
    /// available to occur.
    let isEnabled places tid transitions =
        // FIXME: Due to naive implementation it only checks that a token exist
        transitions 
        |> inputs tid
        |> List.forall (fun pid -> places |> Place.hasTokens pid)

    /// Given Places and Transitions it returns new Transitions with only the 
    /// ones available to occur.
    let enabledFor places (Transitions transitions) =  
        Transitions <| 
            (transitions 
            |> Map.filter (fun tid _ -> 
                isEnabled places tid (Transitions transitions)))
    

type Transitions with
    /// Reimplements the way of showing the Net
    member this.Show = 
        let (Transitions transitions) = this

        transitions |> Map.toList