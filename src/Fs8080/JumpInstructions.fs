module Fs8080.JumpInstructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory
open Fs8080.Instructions

// RET if Z flag is not set
let rnz state memory =
    if state.FLAGS &&& FlagMask.Z = 0uy then
        let pc = {
            High = (fetch (state.SP+1us) memory);
            Low = (fetch state.SP memory);
        }

        set16 PC pc state
        |> set16 SP (state.SP + 2us)
        |> incWC 11
    else
        incPC 1us state
        |> incWC 5

// Jump to address if Z flag is not set
let jnz (address: DWord) state =
    let nextpc = 
        if (state.FLAGS &&& FlagMask.Z) = 0uy
        then address.Value
        else state.PC.Value + 3us

    incPC nextpc state
    |> incWC 10

// Jump to address
let jmp address state =
    { state with PC = address }
    |> incWC 10        

// RET if Z flag is set
let rz state memory =
    if state.FLAGS &&& FlagMask.Z = FlagMask.Z then
        let pc = {
            High = (fetch (state.SP+1us) memory);
            Low = (fetch state.SP memory);
        }

        set16 PC pc state
        |> set16 SP (state.SP + 2us)
        |> incWC 15
    else
        incPC 1us state
        |> incWC 11

// POP PC off stack and jump to it
let ret state memory =
    let pc = {
        High = (fetch (state.SP+1us) memory);
        Low = (fetch state.SP memory);
    }

    set16 PC pc state
    |> set16 SP (state.SP + 2us)
    |> incWC 1

// Jump to address if Z flag is set
let jz (address: DWord) state =
    let nextpc = 
        if (state.FLAGS &&& FlagMask.Z) = FlagMask.Z
        then address.Value
        else state.PC.Value + 3us

    incPC nextpc state
    |> incWC 1

// Push PC to stack and jump to address
let call address state =
    let nextpc = state.PC + 3us

    let memchanges = [
        (state.SP - 1us), nextpc.Low;
        (state.SP - 2us), nextpc.High;
    ]

    { state with PC = address; SP = state.SP - 2us }
    |> incWC 17
    |> fun state -> (state, memchanges)

// CALL if Z is not set
let cnz address state =
    if (state.FLAGS &&& FlagMask.Z) = 0uy
    then 
        call address state
    else
        incPC 3us state
        |> incWC 11
        |> fun state -> state, []

// CALL if Z is set
let cz address state =
    if (state.FLAGS &&& FlagMask.Z) = FlagMask.Z
    then 
        call address state
    else
        incPC 3us state
        |> incWC 11
        |> fun state -> state, []

// RET if C is not set
let rnc state memory = 
    if state.FLAGS &&& FlagMask.C = 0uy then
        let pc = {
            High = (fetch (state.SP+1us) memory);
            Low = (fetch state.SP memory);
        }

        set16 PC pc state
        |> set16 SP (state.SP + 2us)
        |> incWC 11
    else
        incPC 1us state
        |> incWC 5

// Jump to address if C flag is not set
let jnc address state =
    let pc = 
        if state.FLAGS &&& FlagMask.C = 0uy then
           address
        else
            state.PC + 3us

    set16 PC pc state
    |> incWC 10

// CALL if C flag is not set
let cnc address state =
    if (state.FLAGS &&& FlagMask.C) = 0uy
    then 
        call address state
    else
        incPC 3us state
        |> incWC 11
        |> fun state -> state, []