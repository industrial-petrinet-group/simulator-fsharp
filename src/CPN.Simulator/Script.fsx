#load "./Operators.fs" 

#load "./Domain/ColorSets/Common.fs"
#load "./Domain/ColorSets/Unit.fs"
#load "./Domain/ColorSet.fs"
#load "./Domain/MultiSet.fs"
#load "./Domain/Domain.fs"

#load "./SampleNets.fs"
#load "./Runtime.fs" 


open CPN.Simulator;;

let y = SampleNets.notSoSimpleNet |> Runtime.step
let x = SampleNets.notSoSimpleNet |> Runtime.allSteps |> Seq.length
let z = SampleNets.notSoSimpleNet |> Runtime.allSteps
let b = z |> Seq.tail |> Seq.head = (z |> Seq.last)