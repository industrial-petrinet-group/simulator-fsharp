#nowarn "025"
namespace CPN.Simulator.Tests.Domain

open CPN.Simulator.Domain
open CPN.Simulator.Domain.ColorSets

module Setup =
    let declarations () =
        let (Ok unitCS') = UnitCS.create <| Some "none"
        let (Ok boolCS') = BoolCS.create <| Some ("none", "whole")
        Declaration.update [ (CS "unit'", unitCS' :> IColorSet)
                             (CS "bool'", boolCS' :> IColorSet) ]