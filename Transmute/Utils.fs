module Utils

open System

let formatTime ms =
    if ms > 1000.0 then
        $"%.2f{ms / 1000.0} s"
    else
        $"%.1f{ms} ms"

let trimWhitespace (s: string) = s.Trim()

let inline time fn =
    let start = DateTime.Now
    let result = fn()
    let stop = DateTime.Now
    let milliseconds = (stop - start).TotalMilliseconds
    result, milliseconds
