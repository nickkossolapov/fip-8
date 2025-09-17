open Fip8.Instructions
open Raylib_cs

let ops = getDecodedInstructions "D:/test-files/IBM Logo.ch8"

Raylib.InitWindow (800, 480, "Fip-8")

// let runProgramLoop () : bool =
//     not (CBool.op_Implicit (Raylib.WindowShouldClose ()))
//
// while runProgramLoop () do
//     Raylib.BeginDrawing ()
//     Raylib.ClearBackground Color.White
//     Raylib.DrawText ("Hello world", 12, 12, 20, Color.Black)
//     Raylib.EndDrawing ()
//
// Raylib.CloseWindow ()
