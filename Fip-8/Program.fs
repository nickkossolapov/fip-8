open Fip8
open Fip8.Cpu
open Fip8.Instructions
open Fip8.Timing
open Raylib_cs

Display.init ()

let rom = readRom "D:/test-files/IBM Logo.ch8"
let mutable state = createCpuState rom, createTimingState ()

let runProgramLoop () : bool =
    not (CBool.op_Implicit (Raylib.WindowShouldClose ()))

while runProgramLoop () do
    state <- stepEmulation state

    Display.draw (fst state).Screen

Raylib.CloseWindow ()
