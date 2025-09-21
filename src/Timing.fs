module Fip8.Timing

open System.Diagnostics

let targetInstrPerSecond = 700.0
let msPerInstruction = 1000.0 / targetInstrPerSecond
let msPerTimerTick = 1000.0 / 60.0 // CHIP-8 decrements at 60Hz

type TimingState =
    { Stopwatch: Stopwatch
      LastTime: float
      InstructionAccumulator: float
      TimerAccumulator: float
      InstructionsForTick: int
      TimerTicks: int }

let createTimingState () =
    { Stopwatch = Stopwatch ()
      LastTime = 0.0
      InstructionAccumulator = 0.0
      TimerAccumulator = 0.0
      InstructionsForTick = 0
      TimerTicks = 0 }

let getNextTimingState (state: TimingState) =
    if not state.Stopwatch.IsRunning then
        state.Stopwatch.Start ()

    let now = float state.Stopwatch.ElapsedMilliseconds
    let dt = now - state.LastTime

    // Instruction scheduling
    let accInstr = state.InstructionAccumulator + dt
    let numInstr = int (accInstr / msPerInstruction)
    let accInstr' = accInstr - float numInstr * msPerInstruction

    // 60 Hz timer scheduling
    let accTimer = state.TimerAccumulator + dt
    let numTimerTicks = int (accTimer / msPerTimerTick)
    let accTimer' = accTimer - float numTimerTicks * msPerTimerTick

    { state with
        LastTime = now
        InstructionAccumulator = accInstr'
        TimerAccumulator = accTimer'
        InstructionsForTick = numInstr
        TimerTicks = numTimerTicks }
