module Fs8080.ControlInstructions

open Fs8080.Types
open Fs8080.Registers

// No operation
let nop cpu =
     incPC 1us cpu
     |> incWC 4

// Halt CPU
let hlt cpu =
    { cpu with State = State.Halted }
    |> incPC 1us
    |> incWC 7