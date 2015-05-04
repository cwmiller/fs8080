module Fs8080.MoveInstructions

open Fs8080.Types
open Fs8080.Memory
open Fs8080.Registers
open Fs8080.Instructions

// Loads 16bit value into register BC, DE, HL, or SP
let lxi register value state =
    set16 register value state
    |> incPC 3us
    |> incWC 10

// Loads 16bit value from HL into memory
let shld (address: DWord) state =
    let memChanges = [
        (address, (get8 L state));
        ((address + 1us), (get8 H state));
    ]

    incPC 3us state
    |> incWC 16
    |> fun state -> (state, memChanges)

// Loads 16bit value from memory into HL
let lhld address state memory =
    set8 L (fetch address memory) state
    |> set8 H (fetch (address + 1us) memory)
    |> incPC 3us
    |> incWC 16

// Copy 8bit value from A into address in BC or DE
let stax register state =
    let address = get16 register state

    incPC 1us state
    |> incWC 7
    |> fun state -> state, [(address, state.A)]

// Load 8bit value into register
let mvi register value state  =
    set8 register value state
    |> incPC 2us
    |> incWC 7

// Load value from address in 16bit register to A
let ldax register state memory =
    get16 register state
    |> fun address -> fetch address memory
    |> fun value -> set8 A value state
    |> incPC 1us
    |> incWC 7

// Copy value from A to memory address
let sta address state =
    incPC 3us state 
    |> incWC 13
    |> fun state -> (state, [(address, state.A)])

// Copy 8bit value into memory address in HL
let mvi_m value state =
    let address = get16 HL state

    incPC 2us state
    |> incWC 10
    |> fun state -> (state, [(address, value)])

// Copy contents of memory address into A
let lda address state memory =
    fetch address memory
    |> fun value -> set8 A value state
    |> incPC 3us
    |> incWC 13

// Copy 8bit value from register to register
let mov_r_r dest src state =
    copy8 src dest state
    |> incPC 1us
    |> incWC 5

// Copy 8bit value from address HL to register
let mov_r_m dest state memory =
    get16 HL state
    |> fun address -> fetch address memory
    |> fun value -> set8 dest value state
    |> incPC 1us
    |> incWC 7

// Copy 8bit value from register into address HL
let mov_m_r register state =
    let address = get16 HL state
    let value = get8 register state

    incPC 1us state
    |> incWC 7
    |> fun state -> state, [(address, value)]

// Copy value pointed by SP to register and increment SP
let pop register state memory =
    let value = {
        High = fetch (state.SP + 1us) memory;
        Low = fetch state.SP memory;
    }

    set16 register value state
    |> fun state -> { state with SP = state.SP + 2us }
    |> incPC 1us
    |> incWC 10

// Copy value in register onto stack
let push register state =
    let value = get16 register state
    let memchanges = [((state.SP - 2us), value.Low); ((state.SP - 1us), value.High)]

    { state with SP = state.SP - 2us }
    |> incPC 1us
    |> incWC 11
    |> fun state -> (state, memchanges)

// Exchange Stack and HL
let xthl state memory =
    let hl = get16 HL state
    let stack = {
        High = fetch (state.SP + 1us) memory;
        Low = fetch state.SP memory;
    }

    let memchanges = [
        state.SP, hl.Low;
        (state.SP + 1us), hl.High;
    ]

    { state with H = stack.High; L = stack.Low }
    |> incPC 1us
    |> incWC 18
    |> fun state -> (state, memchanges)