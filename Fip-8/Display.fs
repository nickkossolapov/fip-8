module Fip8.Display

open Fip8.Chip8
open Raylib_cs

module private State =
    let scale = 20
    let width, height = chipWidth * scale, chipHeight * scale
    let mutable texture = Unchecked.defaultof<Texture2D>
    let texColorBuffer = Array.zeroCreate<Color> (width * height)

open State

let private upscaleScreen (screen: bool array) =
    for y in 0 .. chipHeight - 1 do
        for x in 0 .. chipWidth - 1 do
            let pixel = screen.[y * chipWidth + x]
            let color = if pixel  then Color.White else Color.Black

            // Add 2 pixel padding between CHIP-8 pixels for a more CRT feel 
            for dy in 1 .. scale - 2 do
                for dx in 1 .. scale - 2 do
                    let sx = x * scale + dx
                    let sy = y * scale + dy
                    texColorBuffer.[sy * width + sx] <- color
                    
let init () =
    Raylib.InitWindow(width, height, "FIP-8")
    let image = Raylib.GenImageColor(width, height, Color.Black)
    texture <- Raylib.LoadTextureFromImage(image)

let draw screen =
    Raylib.BeginDrawing ()
    
    upscaleScreen screen

    Raylib.UpdateTexture (texture, texColorBuffer)
    Raylib.DrawTexture(texture, 0, 0, Color.White);
    
    Raylib.EndDrawing ()    



