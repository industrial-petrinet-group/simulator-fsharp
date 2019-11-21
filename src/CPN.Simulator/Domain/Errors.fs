﻿namespace CPN.Simulator.Domain

/// Type representing ColorSet Errors
type ColorSetErrors = 
    | NotUsable of func: string
    | InvalidInitialState of message: string
    | InvalidValue of value: string
    | OutOfRangeValue of value: string
    | OutOfRangeIndex of index: obj

/// Type representing MultiSet Errors
type MultiSetErrors =
    | UnexpectedError of msg: string
    | InsufficientTokens

/// Type representing Place Errors
type PlaceErrors =
    | UnexpectedError of msg: string
    | InexistenPid of pid: obj * places: obj
    | InsufficientTokensOn of pid: obj * places: obj

/// Errors for the whole Domain 
type Errors =
    | CSErrors of ColorSetErrors
    | MSErrors of MultiSetErrors
    | PErrors of PlaceErrors
