namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Interface implemented by all ColorSets
type ColorSet =
    /// Return the Meta Data asociated with the colorset
    abstract member Name : string 
    
    /// Return the base initial value for this colorset.
    abstract member Init : CSValue

    /// Given a supposed member it checks if is an actual member of the set and
    /// return it's value if it is.
    abstract member Deserialize : string -> Result<CSValue, Errors>
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string colorset value if it is. 
    abstract member Serialize : CSValue -> Result<string, Errors>
    
    /// Given a value of the type it checks if it's a legal one
    abstract member IsLegal : CSValue -> bool    
    
    /// Return a list of all posible values for this colorset.
    abstract member All : Result<CSValue list, Errors>
    
    /// Return the number of different vaules in this colorset.
    abstract member Size : Result<int, Errors>
    
    /// Return the ordinal position of every value in this colorset.
    abstract member Ordinal : CSValue -> Result<int, Errors>
    
    /// Return the actual value for the given position in this colorset.
    abstract member Color : int -> Result<CSValue, Errors>
    
    /// Return a random value of this colorset.
    abstract member Random : Result<CSValue, Errors>



/// Module implementing ColorSet's operations
module ColorSet =
    /// Return an empty colorset
    let empty = VoidCS :> IColorSet<unit>
    
    /// Given a supposed member it checks if is an actual member of the 
    /// colorset and return it's value if it is. 
    let inline colorValue colorString (cs: IColorSet<_>) = 
        cs.ColorValue colorString
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string color set value if it is. 
    let inline colorString colorValue (cs: IColorSet<_>) = 
        cs.ColorString colorValue
    
    /// Return a random value of this colorset.
    let inline randomValue (cs: IColorSet<_>) =
        cs.Random
    
    /// Return a random value of this colorset as a string.
    let inline randomString (cs: IColorSet<_>) =
        cs.Random
        >>= fun randomValue -> colorString randomValue cs
