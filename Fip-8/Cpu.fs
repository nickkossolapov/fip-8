module Fip8.Cpu

open Fip8.Chip8
open Fip8.Instructions
open Fip8.Timing

type VRegister = Byte
type IRegister = Address

type CpuState =
    { V: VRegister array
      I: IRegister
      Stack: Address array
      SP: uint8
      PC: Address
      Screen: bool array
      Memory: uint8 array
      Delay: uint8 }

let createCpuState (rom: uint8 array) =
    let memory = Array.zeroCreate 4096
    Array.blit rom 0 memory (int romStart) rom.Length

    { V = Array.create 16 (Byte 0uy)
      I = Address 0x0us
      Stack = [||]
      SP = 0uy
      PC = Address romStart
      Screen = Array.zeroCreate (screenWidth * screenHeight)
      Memory = memory
      Delay = 255uy }

let private updateScreen (state: CpuState) (VIndex vx) (VIndex vy) (Nibble n) =
    let newScreen = Array.copy state.Screen
    let newV = Array.copy state.V

    newV[int 0xF] <- Byte 0uy

    let Byte xRaw, Byte yRaw = state.V[vx], state.V[vy]
    let x, y = int (xRaw % (uint8 screenWidth)), int (yRaw % (uint8 screenHeight))

    let (Address a) = state.I

    let tryDrawPixel row bitIndex =
        if (x + bitIndex) < screenWidth && (y + row) < screenHeight then
            let screenIndex = (y + row) * screenWidth + x + bitIndex

            if state.Screen[screenIndex] then
                newV[int 0xF] <- Byte 1uy
                newScreen[screenIndex] <- false
            else
                newScreen[screenIndex] <- true

    for row in 0 .. (int n) - 1 do
        let spriteData = state.Memory[(int a) + row]

        for bitIndex = 0 to 7 do
            let mask = 128uy >>> bitIndex
            let pixelOn = (spriteData &&& mask) <> 0uy

            if pixelOn then
                tryDrawPixel row bitIndex

    { state with
        Screen = newScreen
        V = newV }

let private applyDelayTicks (cpu: CpuState) (ticks: int) =
    if ticks <= 0 || cpu.Delay = 0uy then
        cpu
    else
        let remaining = int cpu.Delay - ticks

        { cpu with
            Delay = if remaining <= 0 then 0uy else uint8 remaining }

let private execute (state: CpuState) (instr: Instruction) =
    let newState = { state with PC = state.PC + 2us }

    match instr with
    | ClearScreen ->
        { newState with
            Screen = Array.zeroCreate<bool> (screenWidth * screenHeight) }
    | Jump address -> { newState with PC = address }
    | SetVX (VIndex v, byte) ->
        let newV = Array.copy state.V
        newV[v] <- byte
        { newState with V = newV }
    | AddToVx (VIndex v, byte) ->
        let newV = Array.copy state.V
        newV[v] <- state.V[v] + byte
        { newState with V = newV }
    | SetI address -> { newState with I = address }
    | Display (vx, vy, n) -> updateScreen newState vx vy n
    | Unknown rawInstr ->
        printfn $"Unknown instruction: 0x%04X{rawInstr}"
        state

let stepEmulation fetch (cpuState, timingState) =
    let timingState' = getNextTimingState timingState
    let cpuState' = applyDelayTicks cpuState timingState.TimerTicks

    let cpuState'' =
        Seq.init timingState'.InstructionsForTick id
        |> Seq.fold (fun s _ -> fetch s.PC |> execute s) cpuState'

    cpuState'', timingState'
