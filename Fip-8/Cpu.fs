module Fip8.Cpu

open Fip8.Chip8
open Fip8.Instructions

type VRegister = Byte
type IRegister = Address

type CpuState =
    { V: VRegister array
      I: IRegister
      Stack: Address array
      SP: uint8
      PC: Address
      Screen: bool array
      Memory: uint8 array }

let private updateScreen (state: CpuState) (VIndex vx) (VIndex vy) (Nibble n) =
    let newScreen = Array.copy state.Screen
    let newV = Array.copy state.V

    newV.[int 0xF] <- Byte 0uy

    let Byte xRaw, Byte yRaw = state.V.[vx], state.V.[vy]
    let x, y = xRaw % 64uy, yRaw % 32uy

    let (Address a) = state.I

    let tryDrawPixel row bitIndex =
        if (y + row) > (uint8 screenHeight) && (x + bitIndex) > (uint8 screenWidth) then
            let screenIndex = (y + row) * (uint8 screenHeight) + x + bitIndex |> int

            if state.Screen[screenIndex] then
                newV.[int 0xF] <- Byte 1uy
                state.Screen.[screenIndex] <- false
            else
                state.Screen.[screenIndex] <- true

    for row in 0 .. (int n) do
        let spriteData = state.Memory.[(int a) + row]

        for bitIndex = 0 to 7 do
            let mask = 128uy >>> bitIndex
            let pixelOn = (spriteData &&& mask) <> 0uy

            if pixelOn then
                tryDrawPixel (uint8 row) (uint8 bitIndex)

    { state with
        Screen = newScreen
        V = newV }

let execute (state: CpuState) (instr: Instruction) =
    let newState = { state with PC = state.PC + 2us }

    match instr with
    | ClearScreen ->
        { newState with
            Screen = Array.zeroCreate<bool> (screenWidth * screenHeight) }
    | Jump address -> { newState with PC = address }
    | SetVX (VIndex v, byte) ->
        let newV = Array.copy state.V
        newV.[v] <- byte
        { newState with V = newV }
    | AddToVx (VIndex v, byte) ->
        let newV = Array.copy state.V
        newV.[v] <- state.V.[v] + byte
        { newState with V = newV }
    | SetI address -> { newState with I = address }
    | Display (vx, vy, n) -> updateScreen state vx vy n
    | Unknown rawInstr ->
        printfn $"Unknown instruction: 0x%04X{rawInstr}"
        state
