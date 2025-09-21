module Fip8.Chip8

let InstructionConfig =
    {| ShiftVyToVx = true
       StoreModifyI = false |}

let romStart = 0x200us // CHIP-8 programs start at 0x200
let stackSize = 16
let memorySize = 4096
let screenWidth, screenHeight = 64, 32

type Address =
    | Address of uint16

    static member (+)(Address a, b: uint16) = Address (a + b)
    static member (-)(Address a, b: uint16) = Address (a - b)

type Byte =
    | Byte of uint8

    static member (+)(Byte l, Byte r) = Byte (l + r)
    static member (-)(Byte b, i: int) = int b - i
    static member (&&&)(Byte l, Byte r) = Byte (l &&& r)
    static member (|||)(Byte l, Byte r) = Byte (l ||| r)
    static member (^^^)(Byte l, Byte r) = Byte (l ^^^ r)
    static member (<<<)(Byte b, s) = Byte (b <<< s)
    static member (>>>)(Byte b, s) = Byte (b >>> s)

type VIndex = VIndex of int
type Nibble = Nibble of uint8
