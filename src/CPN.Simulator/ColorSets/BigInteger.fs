namespace CPN.Simulator.ColorSets

open System
open Common

type BigInteger =
    { low: bigint
      high: bigint }
    
    interface INumeric<bigint> with
        member this.Low = this.low
        member this.High = this.high 

module BigInteger =
    let private typeName = "big integer"
    let private emptyVal = (1I, 0I)
    let private parseFunc =  bigint.TryParse

    /// Return the default actual value for this color set.
    let init = 0I
    
    /// Given an optional initinalization string it return a color set.
    let create lowAndHigh = 
        match Numeric.create typeName emptyVal parseFunc lowAndHigh with
        | Ok (lowVal, highVal) -> Ok { low = lowVal; high = highVal }
        | Error err -> Error err      

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's actual value if it is.
    let colorVal supposedMember bigIntegerCS = 
        Numeric.colorVal emptyVal parseFunc supposedMember bigIntegerCS

    /// Given a value of the type it checks if it's a legal one
    let isLegal n bigIntegerCS = Numeric.isLegal emptyVal n bigIntegerCS

    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is.
    let makeString supposedMember bigIntegerCS = 
         Numeric.makeString emptyVal parseFunc supposedMember bigIntegerCS

    /// Return a list of all posible values for this color set.
    let all bigIntegerCS = Numeric.all emptyVal bigIntegerCS

    /// Return the number of different vaules in this color set.
    let size bigIntegerCS = Numeric.size emptyVal 1I bigIntegerCS

    /// Return the ordinal position of every value in this color set.
    let ordinal n bigIntegerCS = Numeric.ordinal emptyVal n bigIntegerCS

    /// Return the actual value for the given position in this color set.
    let colour n bigIntegerCS = Numeric.colour emptyVal n bigIntegerCS

    /// Return a random value of this color set.
    // TODO: Try to use NextByte for generating Big Integers
    let random = function
        | _ -> Error <| NotUsable "random"
        // | EmptyRange -> Error <| NotUsable "random"
        // | Range(low, high) -> Ok <| rnd.Next(low, high)