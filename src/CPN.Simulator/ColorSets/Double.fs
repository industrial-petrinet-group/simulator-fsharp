namespace CPN.Simulator.ColorSets

open System
open Common
open Numeric

type Double =
    { low: double
      high: double }
      
    interface INumeric<double> with
        member this.Low = this.low
        member this.High = this.high 
    

module Double =
    let private typeName = "double (64bits)"
    let private emptyVal = (1.0, 0.0)
    let private parseFunc =  Double.TryParse

    /// Return the default actual value for this color set.
    let init = 0.0
    
    /// Given an optional initinalization string it return a color set.
    let create lowAndHigh = 
        match Numeric.create typeName emptyVal parseFunc lowAndHigh with
        | Ok (lowVal, highVal) -> Ok { low = lowVal; high = highVal }
        | Error err -> Error err      

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal supposedMember doubleCS = 
        Numeric.colorVal emptyVal parseFunc supposedMember doubleCS

    /// Given a value of the type it checks if it's a legal one
    let isLegal n doubleCS = Numeric.isLegal emptyVal n doubleCS

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let makeString supposedMember doubleCS = 
         Numeric.makeString emptyVal parseFunc supposedMember doubleCS

    //#########################################################################
    // The next functions are not available for Doubles given the infinite 
    // number of memebers between any given two.

    /// Return a list of all posible values for this color set.
    let all doubleCS =  Error <| NotUsable "all"
    /// Return the number of different vaules in this color set.
    let size doubleCS =  Error <| NotUsable "size"
    /// Return the ordinal position of every value in this color set.
    let ordinal n doubleCS =  Error <| NotUsable "ordinal"
    /// Return the actual value for the given position in this color set.
    let colour n doubleCS =  Error <| NotUsable "colour"

    //#########################################################################

    /// Return a random value of this color set.
    // TODO: implement random for Big Integers
    let random = function
        | _ -> Error <| NotUsable "random"
