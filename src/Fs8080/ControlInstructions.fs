module Fs8080.ControlInstructions

open Fs8080.Types
open Fs8080.Registers

// No operation
let nop cpu = (incPC 1us cpu), 4

// Halt CPU
let hlt cpu =
    { cpu with State = State.Halted }
    |> incPC 1us, 7

// Disable interrupts
let di cpu =
    { cpu with INTE = false }
    |> incPC 1us, 4

// Enable interrupts
let ei cpu =
    { cpu with INTE = true }
    |> incPC 1us, 4