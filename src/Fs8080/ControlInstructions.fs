module Fs8080.ControlInstructions

open Fs8080.Types
open Fs8080.Registers

// No operation
let nop state =
     incPC 1us state
     |> incWC 4

// Halt CPU
let hlt state =
    { state with RunState = RunState.Halted }
    |> incPC 1us
    |> incWC 7