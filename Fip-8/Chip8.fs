module Fip8.Chip8

let romStart = 0x200us // CHIP-8 programs start at 0x200

let chipWidth, chipHeight =  64, 32

type Address = Address of uint16
    with
        static member (+) (Address a, b: uint16) = Address (a + b)

type Byte = Byte of uint8
    with
        static member (+) (Byte l, Byte r) = Byte (l + r)
        
type VIndex = VIndex of int
type Nibble = Nibble of uint8
