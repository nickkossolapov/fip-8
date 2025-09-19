open Fip8
open Fip8.Chip8
open Fip8.Cpu
open Fip8.Instructions
open Fip8.Timing
open Raylib_cs



Display.init ()

let rom = readRom "D:/test-files/IBM Logo.ch8"
let instructions = getDecodedInstructions rom
let fetch (Address pc) = getInstruction instructions (int pc)
let mutable cpuState = createCpuState rom
let mutable timingState = createTimingState ()

let runProgramLoop () : bool =
    not (CBool.op_Implicit (Raylib.WindowShouldClose ()))

while runProgramLoop () do
    timingState <- getNextTimingState timingState

    for _ in 0 .. timingState.InstructionsForTick do
        cpuState <- fetch cpuState.PC |> execute cpuState

    Display.draw cpuState.Screen

Raylib.CloseWindow ()
