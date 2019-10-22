module Tests

open Expecto
open Swensen.Unquote

[<Tests>]
let tests =
  testList "samples" [
    testCase "universe exists" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."

    testCase "should fail" <| fun _ ->
      let subject = false
      Expect.isTrue subject "I should fail because the subject is false."

    testCase "Simulator Exists" <| fun _ ->
      let sim = CPN.Simulator.Simulator()

      sim.X =! "F#"
  ]