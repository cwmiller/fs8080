module Fs8080.Memory

// Retrieve a value from memory
let fetch (address:uint16) memory =
    Array.get<byte> memory (int address)

// Set a value in memory
let store (address:uint16) (value:byte) memory =
    Array.set<byte> memory (int address) value