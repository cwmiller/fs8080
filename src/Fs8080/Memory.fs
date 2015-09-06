module Fs8080.Memory

open Fs8080.Types

// Retrieve a value from memory
let fetch address (memory: Memory) =
    if memory.ContainsKey(address)
    then memory.Item(address)
    else 0x00uy

// Set a value in memory
let store address (value:byte) (memory:Memory) =
    memory.Add(address, value)

let rec fill (address: uint16) bytes (memory: Memory) =
    match bytes with
    | head :: tail -> fill (address + 1us) tail <| memory.Add(address, head)
    | [] -> memory
        