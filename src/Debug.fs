module Fip8.Debug

open System.Diagnostics


let private stopwatch = Stopwatch ()
let mutable private lastPrint = 0L

let print (msg: string) =
    if not stopwatch.IsRunning then
        stopwatch.Start ()

    let now = stopwatch.ElapsedMilliseconds

    if now - lastPrint >= 1000L then
        printfn $"{msg}"
        lastPrint <- now
