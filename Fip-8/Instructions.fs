module Fip8.Instructions

open System.IO
open Fip8.Chip8


let private getAddress (byte: uint16) = (byte &&& 0x0FFFus) |> Address

let private getVx (byte: uint16) =
    byte &&& 0x0F00us >>> 8 |> int |> VIndex

let private getVy (byte: uint16) =
    byte &&& 0x00F0us >>> 4 |> int |> VIndex

let private getNibble (byte: uint16) = byte &&& 0x000Fus |> uint8 |> Nibble

let private getByte (byte: uint16) = byte &&& 0x00FFus |> uint8 |> Byte

type Instruction =
    | ClearScreen // 0x0OE0
    | Jump of Address // 0x1
    | SetVX of VIndex * Byte // 0x6
    | AddToVx of VIndex * Byte // 0x7
    | SetI of Address // 0xA
    | Display of VIndex * VIndex * Nibble // 0xD
    | Unknown of uint16

let private decode (instr: uint16) =
    let opcode = (instr &&& 0xF000us) >>> 12

    match opcode with
    | 0x0us when instr = 0x00E0us -> ClearScreen
    | 0x1us ->
        let nnn = getAddress instr
        Jump nnn
    | 0x6us ->
        let vx = getVx instr
        let nn = getByte instr
        SetVX (vx, nn)
    | 0x7us ->
        let vx = getVx instr
        let nn = getByte instr
        AddToVx (vx, nn)
    | 0xAus ->
        let nnn = getAddress instr
        SetI nnn
    | 0xDus ->
        let vx = getVx instr
        let vy = getVy instr
        let n = getNibble instr
        Display (vx, vy, n)
    | _ -> Unknown instr

let readRom path = File.ReadAllBytes path

let getDecodedInstructions (rom: uint8 array) =
    rom
    |> Array.chunkBySize 2
    |> Array.map (function
        | [| left; right |] -> (uint16 left <<< 8) ||| uint16 right
        | _ -> failwith "Invalid ROM file") // TODO handle odd sized files more elegantly
    |> Array.mapi (fun i instr ->
        let address = romStart + uint16 (i * 2) |> Address // CHIP-8 programs start at 0x200
        address, decode instr)

let getInstruction (instructions: (Address * Instruction) array) pc =
    let (addr, instr) = instructions[(pc - int 0x200us) / 2]

    instr
