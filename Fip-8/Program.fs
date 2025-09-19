open Fip8
open Fip8.Chip8
open Fip8.Cpu
open Fip8.Instructions
open Raylib_cs

let runProgramLoop () : bool =
    not (CBool.op_Implicit (Raylib.WindowShouldClose ()))

Display.init ()

let rom = readRom "D:/test-files/IBM Logo.ch8"
let instructions = getDecodedInstructions rom
let fetch (Address pc) = getInstruction instructions (int pc)
let mutable cpuState = createState rom

while runProgramLoop () do
    cpuState <- fetch cpuState.PC |> execute cpuState

    Display.draw cpuState.Screen

Raylib.CloseWindow ()
