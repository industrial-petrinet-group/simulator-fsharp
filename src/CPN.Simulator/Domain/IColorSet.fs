namespace CPN.Simulator.Domain

/// Interface implemented by all ColorSets
type IColorSet =
    /// Return the Meta Data asociated with the colorset
    abstract member Name : string 
    
    /// Return the base initial value for this colorset.
    abstract member Init : Color

    /// Given a supposed member it checks if is an actual member of the set and
    /// return it's value if it is.
    abstract member Deserialize : string -> Result<Color, Errors>
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string colorset value if it is. 
    abstract member Serialize : Color -> Result<string, Errors>
    
    /// Given a value of the type it checks if it's a legal one
    abstract member IsLegal : Color -> Result<bool, Errors>    
    
    /// Return a list of all posible values for this colorset.
    abstract member All : Result<Color list, Errors>
    
    /// Return the number of different vaules in this colorset.
    abstract member Size : Result<int, Errors>
    
    /// Return the ordinal position of every value in this colorset.
    abstract member Ordinal : Color -> Result<int, Errors>
    
    /// Return the actual value for the given position in this colorset.
    abstract member Color : int -> Result<Color, Errors>
    
    /// Return a random value of this colorset.
    abstract member Random : Result<Color, Errors>

