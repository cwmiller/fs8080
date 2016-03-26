module Fs8080.Memory

open Fs8080.Types

// Retrieve a value from memory
let fetch address (memory: Memory) =
    if memory.ContainsKey(address)
    then memory.Item(address)
    else 0x00uy

// Retrieve a 16bit value from memory
let fetch16 address memory =
    {
        High = (fetch (address + 1us) memory);
        Low = (fetch address memory);
    }

// Set a value in memory
let store address (value: byte) (memory: Memory) =
    memory.Add(address, value)

// Store a 16bit value in memory
let store16 address (value: DWord) (memory: Memory) =
    store (address + 1us) value.High memory
    |> store address value.Low

// Fill a portion of memory starting at address
let rec fill (address: uint16) bytes (memory: Memory) =
    match bytes with
    | head :: tail -> fill (address + 1us) tail <| memory.Add(address, head)
    | [] -> memory
        