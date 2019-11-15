# simulator-fsharp
F# based coloured petrinet simulator

# Simulator Implementation
## Type System
The type system has evolved; it's not so far of a minimal viable simulator 
without bindings and implementing only unit types. 

### Domain
The types and modules are defined inside `src/CPN.Simulator/Domain`

- ColorSet.fs (Unit)
- MultiSet.fs
- Place.fs
- Transition.fs
- Arc.fs
- Net.fs
- CPN.fs

### Simulator
The modules are defined inside `src/CPN.Simulator`

- SampleNets.fs
- Runtime.fs

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
