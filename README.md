# simulator-fsharp
F# based coloured petrinet simulator

# Prototype Implementation
## Naive Type System
Implemented a Naive type system for representing the petrinet (src/Simulator.fs)

# Colour Sets implemented
## Simple
- [x] Unit
- [x] Boolean
- [x] Integer
- [x] BigInteger (old IntInf)
- [x] Double (old Real)
- [ ] Time
- [ ] String
- [ ] Enumerated
- [ ] Index
## Compound
- [ ] Product
- [ ] Record
- [ ] List
- [ ] Union
- [ ] Subset
- [ ] Alias


# Colour Sets functions implemented for every colour
## All colours
- [x] init (old base)
- [x] isLegal (old legal)
- [x] makeString (old mkstr)
- [ ] makeStringMS (old mkstr_ms)
- [ ] input
- [ ] output
- [ ] input_ms
- [ ] output_ms
## Small colour
- [x] all
- [x] size
- [x] ordinal (old ord)
- [x] colour (old col)
- [x] random (old ran) - pending implementation for BigInteger and Double
