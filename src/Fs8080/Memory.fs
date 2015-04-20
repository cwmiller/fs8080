module Fs8080.Memory

open Fs8080.Types

// Retrieve a value from memory
let fetch (address:DWord) memory =
    Array.get<byte> memory (int address.Value)

// Set a value in memory
let store (address:DWord) (value:byte) memory =
    Array.set<byte> memory (int address.Value) value