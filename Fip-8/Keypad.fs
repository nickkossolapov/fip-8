module Fip8.Keypad

open Fip8.Chip8
open Raylib_cs

(* 
    CHIP-8 keypad is implemented in this format:
    
    1	2	3	C
    4	5	6	D
    7	8	9	E
    A	0	B	F
    
    
    This maps to the following keys on a QWERTY layout:
    
    1	2	3	4
    Q	W	E	R
    A	S	D	F
    Z	X	C	V
*)

let private valueToKeycode =
    Map
        [ 0x1uy, KeyboardKey.One
          0x2uy, KeyboardKey.Two
          0x3uy, KeyboardKey.Three
          0xCuy, KeyboardKey.Four
          0x4uy, KeyboardKey.Q
          0x5uy, KeyboardKey.W
          0x6uy, KeyboardKey.E
          0xDuy, KeyboardKey.R
          0x7uy, KeyboardKey.A
          0x8uy, KeyboardKey.S
          0x9uy, KeyboardKey.D
          0xEuy, KeyboardKey.F
          0xAuy, KeyboardKey.Z
          0x0uy, KeyboardKey.X
          0xBuy, KeyboardKey.C
          0xFuy, KeyboardKey.V ]

let private keycodeToValue =
    valueToKeycode |> Map.toSeq |> Seq.map (fun (k, v) -> v, k) |> Map.ofSeq

let isKeyDown (Byte byte) : bool =
    Map.find byte valueToKeycode |> Raylib.IsKeyDown |> CBool.op_Implicit

let lastKeyPressed () : Byte option =
    let key = enum<KeyboardKey> (Raylib.GetKeyPressed ())

    Map.tryFind key keycodeToValue |> Option.map Byte
