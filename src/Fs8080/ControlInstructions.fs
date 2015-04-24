module Fs8080.ControlInstructions

open Fs8080.Registers

// No operation
let nop state =
     incPC 1us state
     |> incWC 4

// Halt CPU
let hlt state =
    incPC 1us state
    |> incWC 7