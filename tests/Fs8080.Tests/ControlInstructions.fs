module Fs8080.Tests.ControlInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.ControlInstructions

let internal defaultCpu = {
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
    INTE = false;
    State = State.Running;
}

// NOP
[<Test>]
let ``NOP should only affect PC and WC states`` () =
    defaultCpu
    |> nop
    |> fun (cpu, _) -> (cpu.A + cpu.B + cpu.C + cpu.D + cpu.E + cpu.FLAGS)
    |> should equal 0

// HLT
[<Test>]
let ``HLT should set State to Halted`` () =
    defaultCpu
    |> hlt
    |> fun (cpu, _) -> cpu.State 
    |> should equal State.Halted

// DI
[<Test>]
let ``DI should disable interrupts`` () =
    { defaultCpu with INTE = true }
    |> di
    |> fun (cpu, _) -> cpu.INTE
    |> should equal false

// EI
[<Test>]
let ``EI should enable interrupts`` () =
    defaultCpu
    |> ei
    |> fun (cpu, _) -> cpu.INTE
    |> should equal true