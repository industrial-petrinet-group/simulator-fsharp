namespace CPN.Simulator.ColorSets

open System
open Common

type Integer =
    { low: int
      high: int }
     
    interface INumeric<int> with
        member this.Low = this.low
        member this.High = this.high 

module Integer =
    let private typeName = "integer (32bits)"
    let private emptyVal = (1, 0)
    let private parseFunc =  Int32.TryParse

    /// Return the default actual value for this color set.
    let init = 0
    
    /// Given an optional initinalization string it return a color set.
    let create lowAndHigh = 
        match Numeric.create typeName emptyVal parseFunc lowAndHigh with
        | Ok (lowVal, highVal) -> Ok { low = lowVal; high = highVal }
        | Error err -> Error err      

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal supposedMember integerCS = 
        Numeric.colorVal emptyVal parseFunc supposedMember integerCS

    /// Given a value of the type it checks if it's a legal one
    let isLegal n integerCS = Numeric.isLegal emptyVal n integerCS

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let makeString supposedMember integerCS = 
         Numeric.makeString emptyVal parseFunc supposedMember integerCS

    /// Return a list of all posible values for this color set.
    let all integerCS = Numeric.all emptyVal integerCS

    /// Return the number of different vaules in this color set.
    let size integerCS = Numeric.size emptyVal 1 integerCS

    /// Return the ordinal position of every value in this color set.
    let ordinal n integerCS = Numeric.ordinal emptyVal n integerCS

    /// Return the actual value for the given position in this color set.
    let colour n integerCS = Numeric.colour emptyVal n integerCS

    /// Return a random value of this color set.
    let random integerCS = 
        match Numeric.size emptyVal 1 integerCS with
        | Ok _ -> Ok <| rnd.Next(integerCS.low, integerCS.high)
        | Error _ -> Error <| NotUsable "random"