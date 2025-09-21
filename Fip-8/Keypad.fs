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
        [ 0x1, KeyboardKey.One
          0x2, KeyboardKey.Two
          0x3, KeyboardKey.Three
          0xC, KeyboardKey.Four
          0x4, KeyboardKey.Q
          0x5, KeyboardKey.W
          0x6, KeyboardKey.E
          0xD, KeyboardKey.R
          0x7, KeyboardKey.A
          0x8, KeyboardKey.S
          0x9, KeyboardKey.D
          0xE, KeyboardKey.F
          0xA, KeyboardKey.Z
          0x0, KeyboardKey.X
          0xB, KeyboardKey.C
          0xF, KeyboardKey.V ]

let private keycodeToValue =
    valueToKeycode |> Map.toSeq |> Seq.map (fun (k, v) -> v, k) |> Map.ofSeq

let isKeyDown value : bool =
    let checkKey key : bool =
        key |> Raylib.IsKeyDown |> CBool.op_Implicit

    Map.tryFind value valueToKeycode
    |> Option.map checkKey
    |> Option.defaultValue false

let lastKeyPressed () : Byte option =
    let key = enum<KeyboardKey> (Raylib.GetKeyPressed ())

    Map.tryFind key keycodeToValue |> Option.map (fun b -> Byte (uint8 b))
