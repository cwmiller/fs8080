module Fs8080.ControlInstructions

open Fs8080.Types
open Fs8080.Registers

// No operation
let nop cpu = (incPC 1us cpu), 4

// Halt CPU
let hlt cpu =
    { cpu with State = State.Halted }
    |> incPC 1us, 7

// Set A to input device's output
let in' byte cpu =
    { cpu with A = byte }
    |> incPC 2us, 10

let out cpu =
    cpu |> incPC 2us, 10

// Disable interrupts
let di cpu =
    { cpu with INTE = false }
    |> incPC 1us, 4

// Enable interrupts
let ei cpu =
    { cpu with INTE = true }
    |> incPC 1us, 4