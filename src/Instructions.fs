module Fip8.Instructions

open System.IO
open Fip8.Chip8


let private getAddress (address: uint16) = (address &&& 0x0FFFus) |> Address

let private getVx (byte: uint16) =
    byte &&& 0x0F00us >>> 8 |> int |> VIndex

let private getVy (byte: uint16) =
    byte &&& 0x00F0us >>> 4 |> int |> VIndex

let private getNibble (byte: uint16) = byte &&& 0x000Fus |> uint8 |> Nibble

let private getByte (byte: uint16) = byte &&& 0x00FFus |> uint8 |> Byte

type Instruction =
    // 0x00E0
    | ClearScreen
    // 0x00EE
    | Return
    // 0x1nnn
    | Jump of Address
    // 0x2nnn
    | Call of Address
    // 0x3xkk
    | SkipEq of VIndex * Byte
    // 0x4xkk
    | SkipNeq of VIndex * Byte
    // 0x5xy0
    | SkipEqVxVy of VIndex * VIndex
    // 0x6xkk
    | LoadVx of VIndex * Byte
    // 0x7xkk
    | AddVx of VIndex * Byte
    // 0x8xy0
    | LoadVxVy of VIndex * VIndex
    // 0x8xy1
    | OrVxVy of VIndex * VIndex
    // 0x8xy2
    | AndVxVy of VIndex * VIndex
    // 0x8xy3
    | XorVxVy of VIndex * VIndex
    // 0x8xy4
    | AddVxVy of VIndex * VIndex
    // 0x8xy5
    | SubVxVy of VIndex * VIndex
    // 0x8xy6
    | ShiftRight of VIndex * VIndex
    // 0x8xy7
    | SubnVxVy of VIndex * VIndex
    // 0x8xyE
    | ShiftLeft of VIndex * VIndex
    // 0x9xy0
    | SkipNeqVxVy of VIndex * VIndex
    // 0xAnnn
    | LoadI of Address
    // 0xB - Note: not implementing 0xBxnn, otherwise would be JumpOffset of Address | VIndex * Byte to handle SUPER-CHIP
    | JumpV0 of Address
    // 0xCxkk
    | RandomVx of VIndex * Byte
    // 0xDxyn
    | Draw of VIndex * VIndex * Nibble
    // 0xEx9E
    | SkipIfKey of VIndex
    // 0xExA1
    | SkipIfNotKey of VIndex
    // 0xFx07
    | LoadVxDelay of VIndex
    // 0xFx0A
    | WaitKey of VIndex
    // 0xFx15
    | LoadDelayVx of VIndex
    // 0xFx18
    | LoadSoundVx of VIndex
    // 0xFx1E
    | AddI of VIndex
    // 0xFx29
    | LoadFontVx of VIndex
    // 0xFx33
    | StoreBCD of VIndex
    // 0xFx55
    | StoreVxToMemory of VIndex
    // 0xFx65
    | LoadVxFromMemory of VIndex
    // Includes: Sys 0x0nnn
    | Ignored
    | Unknown of uint16

let readRom path = File.ReadAllBytes path

let fetch (memory: uint8 array) (Address pc) =
    let left, right = memory[int pc], memory[int pc + 1]

    (uint16 left <<< 8) ||| uint16 right

let decode (instr: uint16) =
    let opcode = int ((instr &&& 0xF000us) >>> 12)

    match opcode with
    | 0x0 when instr = 0x00E0us -> ClearScreen
    | 0x0 when instr = 0x00EEus -> Return
    | 0x0 -> Ignored
    | 0x1 -> Jump (getAddress instr)
    | 0x2 -> Call (getAddress instr)
    | 0x3 -> SkipEq (getVx instr, getByte instr)
    | 0x4 -> SkipNeq (getVx instr, getByte instr)
    | 0x5 when (instr &&& 0x000Fus = 0us) -> SkipEqVxVy (getVx instr, getVy instr)
    | 0x6 -> LoadVx (getVx instr, getByte instr)
    | 0x7 -> AddVx (getVx instr, getByte instr)
    | 0x8 ->
        match int (instr &&& 0x000Fus) with
        | 0x0 -> LoadVxVy (getVx instr, getVy instr)
        | 0x1 -> OrVxVy (getVx instr, getVy instr)
        | 0x2 -> AndVxVy (getVx instr, getVy instr)
        | 0x3 -> XorVxVy (getVx instr, getVy instr)
        | 0x4 -> AddVxVy (getVx instr, getVy instr)
        | 0x5 -> SubVxVy (getVx instr, getVy instr)
        | 0x6 -> ShiftRight (getVx instr, getVy instr)
        | 0x7 -> SubnVxVy (getVx instr, getVy instr)
        | 0xE -> ShiftLeft (getVx instr, getVy instr)
        | _ -> Unknown instr
    | 0x9 when (instr &&& 0x000Fus = 0us) -> SkipNeqVxVy (getVx instr, getVy instr)
    | 0xA -> LoadI (getAddress instr)
    | 0xB -> JumpV0 (getAddress instr)
    | 0xC -> RandomVx (getVx instr, getByte instr)
    | 0xD -> Draw (getVx instr, getVy instr, getNibble instr)
    | 0xE ->
        match instr &&& 0x00FFus with
        | 0x9Eus -> SkipIfKey (getVx instr)
        | 0xA1us -> SkipIfNotKey (getVx instr)
        | _ -> Unknown instr
    | 0xF ->
        match int (instr &&& 0x00FFus) with
        | 0x07 -> LoadVxDelay (getVx instr)
        | 0x0A -> WaitKey (getVx instr)
        | 0x15 -> LoadDelayVx (getVx instr)
        | 0x18 -> LoadSoundVx (getVx instr)
        | 0x1E -> AddI (getVx instr)
        | 0x29 -> LoadFontVx (getVx instr)
        | 0x33 -> StoreBCD (getVx instr)
        | 0x55 -> StoreVxToMemory (getVx instr)
        | 0x65 -> LoadVxFromMemory (getVx instr)
        | _ -> Unknown instr
    | _ -> Unknown instr
