open Fip8
open Fip8.Chip8
open Fip8.Instructions
open Raylib_cs

let instr = getDecodedInstructions "D:/test-files/IBM Logo.ch8"

let chip8Screen = Array.zeroCreate<bool> (screenWidth * screenHeight)
// Draw "H"
for y in 10 .. 16 do
    chip8Screen.[y * screenWidth + 5] <- true
    chip8Screen.[y * screenWidth + 8] <- true
chip8Screen.[13 * screenWidth + 6] <- true
chip8Screen.[13 * screenWidth + 7] <- true
// Draw "I"
for y in 10 .. 16 do
    chip8Screen.[y * screenWidth + 12] <- true

let runProgramLoop () : bool =
    not (CBool.op_Implicit (Raylib.WindowShouldClose ()))

Display.init ()

while runProgramLoop () do
    Display.draw chip8Screen

Raylib.CloseWindow ()
