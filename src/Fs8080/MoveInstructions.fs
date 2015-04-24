module Fs8080.MoveInstructions

open Fs8080.Types
open Fs8080.Memory
open Fs8080.Registers

// Loads 16 bit value into register BC, DE, HL, or SP
let lxi register state memory =
    let address = {
        High = fetch (state.PC + 2us) memory;
        Low = fetch (state.PC + 1us) memory;
    }

    set16 register address state
    |> incPC 3us
    |> incWC 10

// Load 16bit value from HL into memory address
let shld state memory =
    let address = { 
        High = (fetch (state.PC + 2us) memory);
        Low = (fetch (state.PC + 1us) memory);
    }

    let memChanges = [
        (address, (get8 L state));
        ((address + 1us), (get8 H state));
    ]

    incPC 3us state
    |> incWC 16
    |> fun state -> (state, memChanges)

// Load 16bit value from memory into HL
let lhld state memory =
    let address = { 
        High = (fetch (state.PC + 2us) memory);
        Low = (fetch (state.PC + 1us) memory);
    }

    set8 L (fetch address memory) state
    |> set8 H (fetch (address + 1us) memory)
    |> incPC 3us
    |> incWC 16

// Copy 8bit value from A into address in BC or DE
let stax register state =
    let value = get8 A state
    let address = get16 register state
    (incPC 1us state |> incWC 7, [(address, value)])

// Load 8bit value into register
let mvi register state memory =
    set8 register (fetch (state.PC + 1us) memory) state
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
let sta state memory =
    let address = {
        High = fetch (state.PC + 2us) memory;
        Low = fetch (state.PC + 1us) memory
    }

    let value = get8 A state

    incPC 3us state 
    |> incWC 13
    |> fun state -> (state, [(address, value)])

// Copy 8bit value into memory address in HL
let mvi_m state memory =
    let address = get16 HL state
    let value = fetch (state.PC + 1us) memory

    incPC 2us state
    |> incWC 10
    |> fun state -> (state, [(address, value)])

// Copy contents of memory address into A
let lda state memory =
    let address = {
        High = fetch (state.PC + 2us) memory;
        Low = fetch (state.PC + 1us) memory;
    }

    set8 A (fetch address memory) state
    |> incPC 3us
    |> incWC 13

// Copy 8bit value from register to register
let mov_r_r dest src state =
    copy8 src dest state
    |> incPC 1us
    |> incWC 5

// Copy 8bit value from address HL to register
let mov_r_m dest state memory =
    let value = 
        get16 HL state
        |> fun(address) -> fetch address memory

    set8 dest value state
    |> incPC 1us
    |> incWC 7

// Copy 8bit value from register into address HL
let mov_m_r register state =
    let address = get16 HL state
    let value = get8 register state
    (incPC 1us state |> incWC 7, [(address, value)])