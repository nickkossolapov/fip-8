module Fip8.Cpu

open System.Security.Cryptography
open Fip8.Chip8
open Fip8.Instructions
open Fip8.Timing

type VRegister = Byte
type IRegister = Address

type CpuState =
    { V: VRegister array
      I: IRegister
      Stack: Address array
      SP: int
      PC: Address
      Screen: bool array
      Memory: uint8 array
      Delay: uint8 }

let createCpuState (rom: uint8 array) =
    let memory = Array.zeroCreate memorySize
    Array.blit rom 0 memory (int romStart) rom.Length

    { V = Array.create 16 (Byte 0uy)
      I = Address 0x0us
      Stack = Array.zeroCreate stackSize
      SP = 0
      PC = Address romStart
      Screen = Array.zeroCreate (screenWidth * screenHeight)
      Memory = memory
      Delay = 0uy }

module private InstructionImplementations =
    let updateScreen (state: CpuState) (VIndex vx) (VIndex vy) (Nibble n) =
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

    let stackPush state address =
        if state.SP = stackSize then
            failwith $"Stack overflow with {address}"

        let newStack = Array.copy state.Stack
        newStack[state.SP + 1] <- address

        { state with
            Stack = newStack
            SP = state.SP + 1 }

    let stackPop state =
        if state.SP = 0 then
            failwith "Stack underflow"

        { state with
            SP = state.SP - 1
            PC = state.Stack[state.SP] }

    let addVxVy state (VIndex vx) (VIndex vy) =
        let Byte x, Byte y = state.V[vx], state.V[vy]

        let sum = x + y
        let carry = if (int x + int y) > 255 then 0x1uy else 0x0uy

        let newV = Array.copy state.V
        newV[vx] <- Byte (sum &&& 0xFFuy)
        newV[0xF] <- Byte carry

        { state with V = newV }

    let subVxVy state (VIndex vx) (VIndex vy) =
        let Byte x, Byte y = state.V[vx], state.V[vy]

        let noBorrow = if x >= y then 1uy else 0uy
        let diff = int x - int y

        let newV = Array.copy state.V
        newV[vx] <- Byte (byte (diff &&& 0xFF))
        newV[0xF] <- Byte noBorrow

        { state with V = newV }

    let shiftRight state (VIndex vx) (VIndex vy) =
        let newV = Array.copy state.V

        if InstructionConfig.ShiftVyToVx then
            newV[vx] <- newV[vy]

        newV[0xF] <- newV[vx] &&& Byte 1uy
        newV[vx] <- newV[vx] >>> 1

        { state with V = newV }

    let shiftLeft state (VIndex vx) (VIndex vy) =
        let newV = Array.copy state.V

        if InstructionConfig.ShiftVyToVx then
            newV[vx] <- newV[vy]

        newV[0xF] <- (newV[vx] &&& Byte 0x80uy) >>> 7
        newV[vx] <- newV[vx] <<< 1

        { state with V = newV }


    let randomVx state (VIndex vx) (Byte nn) =
        let bytes = RandomNumberGenerator.GetBytes 1
        let r = Byte bytes[0]

        let newV = Array.copy state.V
        newV[vx] <- newV[vx] &&& r

        { state with V = newV }

open InstructionImplementations

let private applyDelayTicks (ticks: int) (cpu: CpuState) =
    if ticks <= 0 || cpu.Delay = 0uy then
        cpu
    else
        let remaining = int cpu.Delay - ticks

        { cpu with
            Delay = if remaining <= 0 then 0uy else uint8 remaining }

let private execute (prev: CpuState) (instr: Instruction) =
    let state = { prev with PC = prev.PC + 2us }

    match instr with
    | ClearScreen ->
        { state with
            Screen = Array.zeroCreate<bool> (screenWidth * screenHeight) }
    | Return -> stackPop state
    | Jump address -> { state with PC = address }
    | Call address -> stackPush state address
    | SkipEq (VIndex v, b) ->
        if state.V[v] = b then
            { state with PC = state.PC + 2us }
        else
            state
    | SkipNeq (VIndex v, b) ->
        if not (state.V[v] = b) then
            { state with PC = state.PC + 2us }
        else
            state
    | SkipEqVxVy (VIndex x, VIndex y) ->
        if state.V[x] = state.V[y] then
            { state with PC = state.PC + 2us }
        else
            state
    | LoadVx (VIndex v, byte) ->
        let newV = Array.copy prev.V
        newV[v] <- byte

        { state with V = newV }
    | AddVx (VIndex v, byte) ->
        let newV = Array.copy prev.V
        newV[v] <- prev.V[v] + byte

        { state with V = newV }
    | LoadVxVy (VIndex x, VIndex y) ->
        let newV = Array.copy prev.V
        newV[x] <- newV[y]

        { state with V = newV }
    | OrVxVy (VIndex x, VIndex y) ->
        let newV = Array.copy prev.V
        newV[x] <- newV[x] &&& newV[y]

        { state with V = newV }
    | AndVxVy (VIndex x, VIndex y) ->
        let newV = Array.copy prev.V
        newV[x] <- newV[x] &&& newV[y]

        { state with V = newV }
    | XorVxVy (VIndex x, VIndex y) ->
        let newV = Array.copy prev.V
        newV[x] <- newV[x] ^^^ newV[y]

        { state with V = newV }
    | AddVxVy (vx, vy) -> addVxVy state vx vy
    | SubVxVy (vx, vy) -> subVxVy state vx vy
    | ShiftRight (vx, vy) -> shiftRight state vx vy
    | SubnVxVy (vx, vy) -> subVxVy state vy vx
    | ShiftLeft (vx, vy) -> shiftLeft state vx vy
    | SkipNeqVxVy (VIndex x, VIndex y) ->
        if not (state.V[x] = state.V[y]) then
            { state with PC = state.PC + 2us }
        else
            state
    | LoadI address -> { state with I = address }
    | JumpV0 (Address a) ->
        let (Byte v0) = state.V[0]

        { state with
            PC = Address (a + uint16 v0) }
    | RandomVx (vx, nn) -> randomVx state vx nn
    | Draw (vx, vy, n) -> updateScreen state vx vy n
    | SkipIfKey vIndex -> failwith "todo"
    | SkipIfNotKey vIndex -> failwith "todo"
    | LoadVxDelay vIndex -> failwith "todo"
    | WaitKey vIndex -> failwith "todo"
    | LoadDelayVx vIndex -> failwith "todo"
    | LoadSoundVx vIndex -> failwith "todo"
    | AddI vIndex -> failwith "todo"
    | LoadFontVx vIndex -> failwith "todo"
    | StoreBCD vIndex -> failwith "todo"
    | StoreVxToMemory vIndex -> failwith "todo"
    | LoadVxFromMemory vIndex -> failwith "todo"
    | Ignored -> state
    | Unknown rawInstr ->
        printfn $"WARN: Unknown instruction: 0x%04X{rawInstr}"
        state

let stepEmulation (prevCpuState, prevTimingState) =
    let timingState = getNextTimingState prevTimingState

    let executeNextInstruction state =
        fetch state.Memory state.PC |> decode |> execute state

    let rec executeInstructionsForStep count state =
        if count = 0 then
            state
        else
            executeInstructionsForStep (count - 1) (executeNextInstruction state)

    let cpuState =
        prevCpuState
        |> applyDelayTicks timingState.TimerTicks
        |> executeInstructionsForStep timingState.InstructionsForTick

    cpuState, timingState
