namespace CPN.Simulator.Domain.ColorSets

open CPN.Simulator.Domain

/// Type representing MetaData for all Color Sets
type CSMetaData = 
    { name: string
      internalType: System.Type
      colorSetHash: int }

type IColorSet<'T> =
    /// Return the Meta Data asociated with the Color Set
    abstract member MetaData : CSMetaData
    
    /// Return the base initial value for this color set.
    abstract member Init : (unit -> 'T)
    
    /// Given a value of the type it checks if it's a legal one
    abstract member IsLegal : 'T -> bool
    
    /// Given a supposed member it checks if is an actual member of the set and
    /// return it's value if it is.
    abstract member ColorValue : string -> Result<'T, Errors>
    
    /// Given a supposed member and a color set it checks if the value is a 
    /// member of the set and return it's string color set value if it is. 
    abstract member ColorString : 'T -> Result<string, Errors>
    
    /// Return a list of all posible values for this color set.
    abstract member All : Result<'T list, Errors>

    /// Return the number of different vaules in this color set.
    abstract member Size : Result<int, Errors>

    /// Return the ordinal position of every value in this color set.
    abstract member Ordinal : 'T -> Result<int, Errors>

    /// Return the actual value for the given position in this color set.
    abstract member Color : int -> Result<'T, Errors>
    
    /// Return a random value of this color set.
    abstract member Random : Result<'T, Errors>

module Common =
    let rnd = System.Random()

    let inline asString cs =
        let allValues = 
            match (^T: (member All: Result<'a list,_>) cs) with
            | Error _ -> ""
            | Ok list -> sprintf "%A" list
    
        sprintf "%s" allValues   
   

