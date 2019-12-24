# simulator-fsharp
F# based coloured petrinet simulator

# Simulator Implementation
## Type System
The type system has evolved; it's a minimal implementation of coulored petrinets
without arc expressions and guards

### Domain
The types and modules are defined inside `src/CPN.Simulator/Domain`

- Color.fs - Implement the abstraction for color values to be used by ColorSets and MultiSets.
- Declarations.fs - Implement the basic declarations and expose an update function for user defined ones.
- ColorSet.fs - Simple ColorSets implemented so far: Unit, Bool, Int, Bigint (old IntInf), Float(old Real)
- MultiSet.fs - Implement the type and operations associated with Multisets, representing the marking of Places
- Place.fs - Represent Places inside a Net, composed by name, and color (implicit) and marking (exlicit)
- Transition.fs - Transitions have embedded the representation not only of them but also of input and output arcs
- CPN.fs - Repesentation of a Colored Petri Net

### Simulator
The modules are defined inside `src/CPN.Simulator`

- SampleNets.fs - Contain Sample Nets for testing
- JsonParser.fs - Implementation of a Parser for Json defined CPNs
- Runtime.fs - Operations of the simulator runtime such as step and allSteps

# Color Sets implemented
## Simple
- [x] Void
- [x] Unit
- [x] Bool
- [x] Int
- [x] Bigint (old IntInf)
- [x] Float (old Real)
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
- [x] serialize (old mkstr)
- [ ] makeStringMS (old mkstr_ms)
- [x] deserialize (old input)
- [ ] output
- [ ] input_ms
- [ ] output_ms
## Small colour
- [x] all
- [x] size
- [x] ordinal (old ord)
- [x] colour (old col)
- [x] random (old ran) - pending implementation for BigInteger and Double
