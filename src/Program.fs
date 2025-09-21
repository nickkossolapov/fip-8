open Fip8
open Fip8.Cpu
open Fip8.Instructions
open Fip8.Timing
open Raylib_cs

Display.init ()

// Test suite from https://github.com/Timendus/chip8-test-suite
let testRoms =
    [| "D:/test-files/1-chip8-logo.ch8"
       "D:/test-files/2-ibm-logo.ch8"
       "D:/test-files/3-corax+.ch8"
       "D:/test-files/4-flags.ch8"
       "D:/test-files/5-quirks.ch8"
       "D:/test-files/6-keypad.ch8"
       "D:/test-files/7-beep.ch8" |]

let rom = readRom testRoms[1]
let mutable state = createCpuState rom, createTimingState ()

let runProgramLoop () : bool =
    not (CBool.op_Implicit (Raylib.WindowShouldClose ()))

while runProgramLoop () do
    state <- stepEmulation state

    Display.draw (fst state).Screen

Raylib.CloseWindow ()
