namespace CPN.Simulator.Domain

open CPN.Simulator.Operators

/// Module implementing Color related operations
module Color =
    /// Return the empty Color
    let empty = Void

    /// Given a ColorSetId and a valur it tries to pack them inside a Color
    let pack csid value =
        let csOrDefault = defaultArg csid

        match box value with
        | :? unit as unitColor -> Ok <| Unit (unitColor, csOrDefault <| CS "unit")
        | :? bool as boolColor -> Ok <| Bool (boolColor, csOrDefault <| CS "bool")
        | :? int as intColor -> Ok <| Int (intColor, csOrDefault <| CS "int")
        | :? bigint as bigintColor -> Ok <| Bigint (bigintColor, csOrDefault <| CS "bigint")
        | :? float as floatColor -> Ok <| Float (floatColor, csOrDefault <| CS "float")
        | :? string as stringColor -> Ok <| String (stringColor, csOrDefault <| CS "string")
        | _ -> Error <| CSErrors (InvalidColor <| sprintf "%A" value)
    
    /// Given a value it tries to pack it in a Color through it's default ColorSetId
    let defaultPack value = pack None value   

    /// Given a Color it unpacks it
    let unpack color =
        match color with
        | Void -> Error <| CSErrors (InvalidColor <| sprintf "void")
        | Unit (unitColor, csid) -> Ok (box unitColor, csid)
        | Bool (boolColor, csid) -> Ok (box boolColor, csid)
        | Int (intColor, csid) -> Ok (box intColor, csid)
        | Bigint (bigintColor, csid) -> Ok (box bigintColor, csid)
        | Float (floatColor, csid) -> Ok (box floatColor, csid)
        | String (stringColor, csid) -> Ok (box stringColor, csid)
    
    /// Given a Color it unpacks it's Color value
    let colorValue = unpack >=> switch fst

    /// Given a Color it unpacks it's ColorSetId
    let colorSetId = unpack >=> switch snd

    /// Given a mapping function it maps a Color to a new one based on the 
    /// defaults ColorSetIds 
    let map (mapping : obj -> 'r) = 
        colorValue >=> switch mapping >=> defaultPack


/// Module implementing ColorSet's operations
module ColorSet =
    /// Given a value it returns a default ColorSetId for that value
    let ofColor value =
        value |> Color.defaultPack >>= Color.colorSetId
    
    /// Return the empty ColorSetId
    let empty = CS "void"
    
    /// Given a supposed member it checks if is an actual member of the 
    /// colorset and return it's value if it is. 
    let deserialize (csid: ColorSetId) supossedMember = 
        csid
        |> Declarations.colorSet (Declarations.defaults)
        >>= fun cs -> cs.Deserialize supossedMember
    
    /// Given a supposed member and a colorset it checks if the value is a 
    /// member of the set and return it's string color set value if it is. 
    let serialize color = 
        color
        |> Color.colorSetId
        >>= Declarations.colorSet (Declarations.defaults)
        >>= fun cs -> cs.Serialize color
    
    /// Return a random value of this colorset.
    let random (cs: IColorSet) = cs.Random
    
    /// Return a random value of this colorset as a string.
    let inline randomAsString (cs: IColorSet) = cs.Random >>= (serialize cs)

   