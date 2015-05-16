module Fs8080.Memory

open Fs8080.Types

// Retrieve a value from memory
let fetch address (memory:Map<uint16, byte>) =
    if memory.ContainsKey(address)
    then memory.Item(address)
    else 0x00uy

// Set a value in memory
let store address (value:byte) (memory:Map<uint16, byte>) =
    memory.Add(address, value)