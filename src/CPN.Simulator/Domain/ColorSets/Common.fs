namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

/// Type representing MetaData for all ColorSets
type CSMetaData = 
    { name: string
      internalType: System.Type
      colorSetHash: int }

/// Interface implemented by all ColorSets
type IColorSet<'T> =
    /// Return the Meta Data asociated with the colorset
    abstract member MetaData : CSMetaData
    
    /// Return the base initial value for this colorset.
    abstract member Init : (unit -> 'T)
    
    /// Given a value of the type it checks if it's a legal one
    abstract member IsLegal : 'T -> bool
    
    /// Given a supposed member it checks if is an actual member of the set and
    /// return it's value if it is.
    abstract member ColorValue : string -> Result<'T, Errors>
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string colorset value if it is. 
    abstract member ColorString : 'T -> Result<string, Errors>
    
    /// Return a list of all posible values for this colorset.
    abstract member All : Result<'T list, Errors>

    /// Return the number of different vaules in this colorset.
    abstract member Size : Result<int, Errors>

    /// Return the ordinal position of every value in this colorset.
    abstract member Ordinal : 'T -> Result<int, Errors>

    /// Return the actual value for the given position in this colorset.
    abstract member Color : int -> Result<'T, Errors>
    
    /// Return a random value of this colorset.
    abstract member Random : Result<'T, Errors>

/// Module implementing common functions for ColorSets
module Common =
    /// Random generator 
    let rnd = System.Random()

    /// Given a IColorSet it returns a String representing it.
    let inline asString (cs: IColorSet<_>) =
        match cs.All with
        | Error _ -> ""
        | Ok list -> sprintf "%A" list
        |> sprintf "%s"
   

