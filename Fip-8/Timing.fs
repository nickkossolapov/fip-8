module Fip8.Timing

open System.Diagnostics

let targetInstrPerSecond = 700.0
let msPerInstruction = 1000.0 / targetInstrPerSecond

type TimingState =
    { Stopwatch: Stopwatch
      LastTime: float
      Accumulator: float
      InstructionsForTick: int }

let createTimingState () =
    { Stopwatch = Stopwatch ()
      LastTime = 0.0
      Accumulator = 0.0
      InstructionsForTick = 0 }

let getNextTimingState (state: TimingState) =
    if not state.Stopwatch.IsRunning then
        state.Stopwatch.Start ()

    let currentTime = float state.Stopwatch.ElapsedMilliseconds
    let dt = currentTime - state.LastTime

    let accumulator = state.Accumulator + dt
    let numInstr = int (accumulator / msPerInstruction)
    let newAccumulator = accumulator - float numInstr * msPerInstruction

    { state with
        LastTime = currentTime
        Accumulator = newAccumulator
        InstructionsForTick = numInstr }
