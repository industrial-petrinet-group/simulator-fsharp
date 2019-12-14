#load "./Operators.fs" 
#load "./Domain/Errors.fs"
#load "./Domain/ColorSets/Common.fs"
#load "./Domain/ColorSets/Void.fs"
#load "./Domain/ColorSets/Unit.fs"
#load "./Domain/ColorSets/Boolean.fs"
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

let (Ok unitCS) = UnitCS.create None

let steps = SampleNets.simpleBooleanNet

//let (Ok unitCS) = Unit.create None
//let unitColour = UnitCS unitCS;;

//let (Ok ms1) = MultiSet.ofString unitColour "1`()++2`()"
//let (Ok ms2) = MultiSet.ofString unitColour "1`()++1`()++1`()"
//let (Ok ms3) = MultiSet.ofString unitColour "1`()++1`()"


let inline specify (placeData: IPlaceData) : PlaceData<_> = 
    let internalType =
        (placeData :?> PlaceData<_>).color.MetaData.internalType

    let resultType =
        typeof<PlaceData<_>>.MakeGenericType internalType

    System.Convert.ChangeType(placeData, resultType) :?> PlaceData<_>

type EspecificList =
    | IntList of int list
    | BoolList of bool list

let listLength = function
    | IntList list -> List.length list
    | BoolList list -> List.length list

let listGenericFunc func = function
    | IntList list -> list |> box |> func |> unbox
    | BoolList list -> list |> box |> func |> unbox

let listGenericFunc2 func list = list |> func

let pdType = typeof<PlaceData<_>>.MakeGenericType typeof<unit>

System.Convert.ChangeType(obj, pdType);

let (x : int) =
    [1; 2; 3]
    |> listGenericFunc2 List.length

let (y : int) =
    [true; false]
    |> listGenericFunc2 List.length

type UniversalListLength = abstract member Eval<'a> : 'a -> 'a

let id : UniversalId =
    { new UniversalId with
        member __.Eval<'a> (x : 'a) : 'a = x }

let id2 =
    { new UniversalId with
        member __.Eval<'a> (x : 'a) : 'a = x }.Eval

type Id3 = Id3 with static member ( $ ) (Id3, x) = x

let a = id.Eval("pepe")
let b = id2 "pepe"
let c = id.Eval(3)
let d = id2 3

// work
let execute (func : UniversalId) (x: obj) =
    match x with
    | :? int as i -> sprintf "Int %i" (func.Eval i)
    | :? bool as b -> sprintf "Bool %b" (func.Eval b)
    | :? string as s -> sprintf "Str %s" (func.Eval s)

// Does not work
let execute2 (func: 'a -> 'a) (x: obj) =
    match x with
    | :? int as i -> sprintf "Int %i" (func i)
    | :? bool as b -> sprintf "Bool %b" (func b)
    | :? string as s -> sprintf "Str %s" (func s)


type ListLength = ListLength with static member ( $ ) (ListLength, x) = x |> List.length

let inline applier f x = f $ x

let listGenericFuncInline func = function
    | IntList list -> applier func list
    | BoolList list -> applier func list

listGenericFuncInline ListLength (IntList [1; 2; 3]) 
listGenericFuncInline ListLength (BoolList [true; false])

//work
let execute3 func (x: obj) =
    match x with
    | :? int as i -> sprintf "Int %i" (applier func i)
    | :? bool as b -> sprintf "Bool %b" (applier func b)
    | :? string as s -> sprintf "Str %s" (applier func s)


execute id 3
execute id true
execute id "pepe"
//execute2 id2 3 "pepe"

execute3 Id3 3
execute3 Id3 true
execute3 Id3 "pepe"

module MSCrate =
    
    let make (ms : MultiSet<'a>) =
        { new MSCrate with
            member __.Apply e = e.Eval ms }
    
    let extract (msCrate : MSCrate) =
        msCrate.Apply 
            { new MSCrateEvaluator<obj> with
                 member __.Eval ms = box ms } |> unbox

    let asString (msCrate : MSCrate) = //MultiSet.asString
        msCrate.Apply 
            { new MSCrateEvaluator<string> with
                member __.Eval ms = ms |> MultiSet.asString }

let steps = SampleNets.simpleBooleanNet |> Runtime.allSteps

steps |> Seq.length

steps 
|> Seq.last 
|> CPN.netMarking 


let (Ok boolCS1) = Boolean.create None
let (Ok boolCS2) = Boolean.create (Some ("none", "whole"))
let boolColour1, boolColour2 = BooleanCS boolCS1, BooleanCS boolCS2

let (Ok ms1) = MultiSet.ofString boolColour1 "1`true++2`false"
let (Ok ms2) = MultiSet.ofString boolColour2 "1`none++1`whole++1`none"
let (Ok ms3) = MultiSet.ofString boolColour1 "1`true++1`false"
let (Ok ms4) = MultiSet.ofString boolColour1 "1`false++1`true++1`false"
let (Ok ms5) = MultiSet.ofString boolColour2 "2`none++1`whole"


SampleNets.randomlyPathedNet |> printfn "%A";;

SampleNets.notSoSimpleNet |> Runtime.allSteps;;



// let (Ok unitCS) = Unit.create None
// let unitColour = UnitCS unitCS
// let unitToken = { qty = 1; value = "()"; colour = unitColour}

// let net1 =
//     let places = 
//         Map.empty.
//             Add(P 1, { name = "P1"; colour = unitColour; marking = [{unitToken with qty = 3}] }).
//             Add(P 2, { name = "P2"; colour = unitColour; marking = [] }).
//             Add(P 3, { name = "P3"; colour = unitColour; marking = [unitToken] }).
//             Add(P 4, { name = "P4"; colour = unitColour; marking = [] }).
//             Add(P 5, { name = "P5"; colour = unitColour; marking = [] })

//     let transitions = 
//         Map.empty.
//             Add(T 1, { name = "T1"; bindings = [] }).
//             Add(T 2, { name = "T2"; bindings = [] })

//     let arcs = 
//         Map.empty.
//             Add(A 1, {expression = ""}).
//             Add(A 2, {expression = ""}).
//             Add(A 3, {expression = ""}).
//             Add(A 4, {expression = ""}).
//             Add(A 5, {expression = ""}).
//             Add(A 6, {expression = ""}).
//             Add(A 7, {expression = ""})
    
//     let net =
//         Map.empty.
//             Add(T 1, 
//                     { i = [(P 1, A 1); (P 2, A 2)]
//                       o = [(P 2, A 3); (P 3, A 4)]}).
//             Add(T 2, 
//                     { i = [(P 3, A 5)]
//                       o = [(P 4, A 5); (P 5, A 7)]})
    

//     CPN (net, (places, transitions, arcs))

// let net2 =
//     let places = 
//         Map.empty.
//             Add(P 1, { name = "P1"; colour = unitColour; marking = [{unitToken with qty = 3}] }).
//             Add(P 2, { name = "P2"; colour = unitColour; marking = [unitToken] }).
//             Add(P 3, { name = "P3"; colour = unitColour; marking = [unitToken] }).
//             Add(P 4, { name = "P4"; colour = unitColour; marking = [] }).
//             Add(P 5, { name = "P5"; colour = unitColour; marking = [] })

//     let transitions = 
//         Map.empty.
//             Add(T 1, { name = "T1"; bindings = [] }).
//             Add(T 2, { name = "T2"; bindings = [] })

//     let arcs = 
//         Map.empty.
//             Add(A 1, {expression = ""}).
//             Add(A 2, {expression = ""}).
//             Add(A 3, {expression = ""}).
//             Add(A 4, {expression = ""}).
//             Add(A 5, {expression = ""}).
//             Add(A 6, {expression = ""}).
//             Add(A 7, {expression = ""})
    
//     let net =
//         Map.empty.
//             Add(T 1, 
//                     { i = [(P 1, A 1); (P 2, A 2)]
//                       o = [(P 2, A 3); (P 3, A 4)]}).
//             Add(T 2, 
//                     { i = [(P 3, A 5)]
//                       o = [(P 4, A 5); (P 5, A 7)]})
    

//     CPN (net, (places, transitions, arcs))

// let a = SampleNets.notSoSimpleNet |> Runtime.step
// let b = net1 |> Runtime.step
// let c = net2 |> Runtime.step;;


type Cell =
    abstract Accept : CellFunc<'R> -> 'R
    abstract Extract : 'a -> 'a

and Cell<'T> = { Items : 'T list }
with
    member cell.Id = cell
    interface Cell with
      member cell.Accept f = f.Invoke<'T> cell
      member cell.Extract cell2 = cell2

and CellFunc<'R> =
    abstract Invoke<'T> : Cell<'T> -> 'R

let pack (cell : Cell<'T>) = cell :> Cell
let unpack (cell : Cell) (f : CellFunc<'R>) : 'R = cell.Accept f
let extract (cell : Cell) = cell.Extract (cell :?> Cell<'a>)

type UniversalId =
    abstract member Eval<'a> : 'a -> 'a

type UniversalFunc<'ret, 'err> =
    abstract member Eval<'a> : 'a -> Result<'ret, 'err>

let func (x : 'a) =
    match box x with
    | :? string as s -> Ok (System.Int32.TryParse s |> snd)
    | _ -> Error "Unimplemented"

let p = pack {Items = ["45";"2"]}


let map (func : UniversalFunc<_,_>) (c : Cell) = unpack c {
    new CellFunc<Cell> with
        member __.Invoke cell =
               pack { Items = cell.Items |> List.map func.Eval |> List.map (function Ok x -> x) }}

let cell = map {
            new UniversalFunc<int, string> with
                member __.Eval x = func x } p

let z () = extract cell

type CSErrors =
    | InvalidColorValue
    | InvalidColorString

type CS =
    abstract member _ColorValue<'T> : string -> (string -> Result<'T, CSErrors>) -> Result<'T, CSErrors>
    abstract member ColorString<'T> : 'T -> Result<string, CSErrors>

type UnitCSData = 
    { unit : string }

type UnitCS =
    | Unit of UnitCSData

    interface CS with
        member __._ColorValue colorString parse = parse colorString
        
        member this.ColorString _colorValue = 
            let (Unit unitCSD) = this in Ok unitCSD.unit
    
    member __.Deserializer _colorString = Ok ()

    member this.ColorValue colorString = (this :> CS)._ColorValue colorString this.Deserializer

type BoolCSData = 
    { falsy : string 
      truthy : string }
    
type BoolCS =
    | Bool of BoolCSData
    
    interface CS with
        member __._ColorValue colorString parse = parse colorString
            
        member this.ColorString colorValue = 
            let (Bool boolCSD) = this

            match box colorValue with
            | :? bool as cv -> Ok <| if cv then boolCSD.truthy else boolCSD.falsy
            | _ -> Error <| InvalidColorValue
            
        
    member private this._Deserializer colorString = 
        let (Bool boolCSD) = this
        let falsyOrTruthy = boolCSD.falsy = colorString,
                            boolCSD.truthy = colorString

        match falsyOrTruthy with
        | true, _ -> Ok false
        | _, true -> Ok true
        | _ -> Error <| InvalidColorString
    
    member this.ColorValue colorString = (this :> CS)._ColorValue colorString this._Deserializer
 

 module ColorSet =

    let inline colorValue colorString cs = 
        (^T: (member ColorValue:_->_) (cs, colorString))

    let inline ofColor< ^T when ^T :> CS> colorValue : ^T=
        match box colorValue with
        | :? unit ->  Unit {unit = "()"} :> CS
        | :? bool ->  Bool {falsy = "falsy"; truthy = "truthy"} :> CS
        |> fun cs -> cs :?> ^T



let unitCS = Unit {unit = "()"} 
let boolCS = Bool {falsy = "falsy"; truthy = "truthy"} 

ColorSet.colorValue "()" unitCS
ColorSet.colorValue "falsy" boolCS


type F2 = 
    member this.Value func = func ()

type MSData =
    { values : Map<string, int>
      color : CS }

type MS =
    | MS of MSData


    