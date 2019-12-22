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
    /// Create the Net containing TransitionIO
    let create (places, transitions, arcs) (transition, inputs, outputs) = ""
     /// Given a serialized array of Transitions containing the name it
     /// generates the Transitions
     //let create transitionSerializedList =
     //    transitionSerializedList
     //    |> Array.fold2 (fun acc id (name, guard) ->
     //        acc |> Map.add (T id) { name = name; guard = guard }
     //    ) Map.empty [| 1..transitionSerializedList.Length |]
     //    |> Transitions   

    /// Returns and empty Net
    let empty = Transitions Map.empty

    /// Given a Net check if it's empty
    let isEmpty (Transitions transitions) = transitions |> Map.isEmpty
    
    /// Given a Net it returns a list of Transitions ID
    let keyList (Transitions transitions) = 
        [] |> Map.foldBack (fun key _ acc -> key :: acc ) transitions

    /// Given a Net it returns a random ordered list of Transitions ID to 
    /// retrive the information of the Net randomly
    let randomKeyList (Transitions transitions) = 
        transitions
        |> Map.fold (fun acc key _ -> key :: acc ) []
        |> randomizeList
    
    /// Given a TransitionId and a Net it returns the corresponding TransitionIO.
    let find tid (Transitions transitions) = transitions |> Map.find tid

    /// Given a Net, add a pair of TransitionId and TransitionIO.
    let add tid transitionIO (Transitions transitions) = 
        Transitions <| transitions.Add(tid, transitionIO)

    /// Given a TransitionId and a Net it returns a PlaceId list of it's inputs.
    let inputs tid transitions =
        let { inputs = inputs } = transitions |> find tid

        inputs |> List.map fst
    
    /// Given a TransitionId and a Net it returns a PlaceId list of it's outputs.
    let outputs tid transitions =
        let { outputs = outputs } = transitions |> find tid

        outputs |> List.map fst

    /// Given Places, a transition Id and a Net it returns if the tid is 
    /// available to occur.
    let isEnabled places tid transitions =
        // FIXME: Due to naive implementation it only checks that a token exist
        transitions 
        |> inputs tid
        |> List.forall (fun pid -> places |> Place.hasTokens pid)

    /// Given Places and a Net it returns a new Net with only the transitions. 
    /// avaliable to occur.
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