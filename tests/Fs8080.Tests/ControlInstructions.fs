module Fs8080.ControlInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.ControlInstructions

let internal defaultState = {
    A = 0uy;
    B = 0uy;
    C = 0uy;
    D = 0uy;
    E = 0uy;
    H = 0uy;
    L = 0uy;
    FLAGS = 0uy;
    SP = { High = 0uy; Low = 0uy; };
    PC = { High = 0uy; Low = 0uy; };
    WC = 0;
}

// NOP
[<Test>]
let ``NOP should only affect PC and WC states`` () =
    defaultState
    |> nop
    |> fun state -> (state.A + state.B + state.C + state.D + state.E + state.FLAGS)
    |> should equal 0

// HLT
[<Test>]
let ``HLT should only affect the PC and WC states`` () =
    defaultState
    |> hlt
    |> fun state -> (state.A + state.B + state.C + state.D + state.E + state.FLAGS)
    |> should equal 0