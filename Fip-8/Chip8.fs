module Fip8.Chip8

let InstructionConfig =
    {| ShiftVyToVx = true
       StoreModifyI = false |}

let romStart = 0x200us // CHIP-8 programs start at 0x200
let fontStart = 0x020us // Convention for font location in memory
let fontCharSize = 5us
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

let fontData =
    [| 0xF0uy
       0x90uy
       0x90uy
       0x90uy
       0xF0uy // 0
       0x20uy
       0x60uy
       0x20uy
       0x20uy
       0x70uy // 1
       0xF0uy
       0x10uy
       0xF0uy
       0x80uy
       0xF0uy // 2
       0xF0uy
       0x10uy
       0xF0uy
       0x10uy
       0xF0uy // 3
       0x90uy
       0x90uy
       0xF0uy
       0x10uy
       0x10uy // 4
       0xF0uy
       0x80uy
       0xF0uy
       0x10uy
       0xF0uy // 5
       0xF0uy
       0x80uy
       0xF0uy
       0x90uy
       0xF0uy // 6
       0xF0uy
       0x10uy
       0x20uy
       0x40uy
       0x40uy // 7
       0xF0uy
       0x90uy
       0xF0uy
       0x90uy
       0xF0uy // 8
       0xF0uy
       0x90uy
       0xF0uy
       0x10uy
       0xF0uy // 9
       0xF0uy
       0x90uy
       0xF0uy
       0x90uy
       0x90uy // A
       0xE0uy
       0x90uy
       0xE0uy
       0x90uy
       0xE0uy // B
       0xF0uy
       0x80uy
       0x80uy
       0x80uy
       0xF0uy // C
       0xE0uy
       0x90uy
       0x90uy
       0x90uy
       0xE0uy // D
       0xF0uy
       0x80uy
       0xF0uy
       0x80uy
       0xF0uy // E
       0xF0uy
       0x80uy
       0xF0uy
       0x80uy
       0x80uy |] // F
