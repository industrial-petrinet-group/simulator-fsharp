namespace CPN.Simulator.Domain

/// Type representing Declaration Errors
type DeclarationErrors =
    | UndeclaredColorSet

/// Type representing ColorSet Errors
type ColorSetErrors = 
    | NotUsable of func: string
    | InvalidInitialState of message: string
    | InvalidValue of value: string
    | InvalidColor of value: string
    | OutOfRangeValue of value: string
    | OutOfRangeIndex of index: obj

/// Type representing MultiSet Errors
type MultiSetErrors =
    | BadFormattedInputString of input: string 
    | UnmatchedColors of colors: string list
    | SubstractorShouldBeLessOrEqual
    | InsufficientTokens

/// Type representing Place Errors
type PlaceErrors =
    | UnexpectedError of msg: string
    | InexistenPid of pid: obj * places: obj
    | InsufficientTokensOn of pid: obj * places: obj

/// Type representing Parsing Errors
type ParsingErrors =
    | PlacesParsingError
    | TransitionsParsingError

/// Errors for the whole Domain 
type Errors =
    | DErrors of DeclarationErrors
    | CSErrors of ColorSetErrors
    | MSErrors of MultiSetErrors
    | PErrors of PlaceErrors
    | PAErrors of ParsingErrors


