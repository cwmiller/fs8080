module Fs8080.MoveInstructions

open Fs8080.Types
open Fs8080.Memory
open Fs8080.Registers

// Loads 16bit value into register BC, DE, HL, or SP
let lxi register value cpu =
    set16 register value cpu
    |> incPC 3us
    |> incWC 10

// Loads 16bit value from HL into memory
let shld (address: DWord) cpu memory =
    let memchange = 
        store address.Value (get8 L cpu) memory
        |> store (address+1us).Value (get8 H cpu)

    incPC 3us cpu
    |> incWC 16
    |> fun cpu -> cpu, memchange

// Loads 16bit value from memory into HL
let lhld (address: DWord) cpu memory =
    set8 L (fetch address.Value memory) cpu
    |> set8 H (fetch (address + 1us).Value memory)
    |> incPC 3us
    |> incWC 16

// Copy 8bit value from A into address in BC or DE
let stax register cpu memory =
    let address = get16 register cpu

    incPC 1us cpu
    |> incWC 7
    |> fun cpu -> cpu, (store address.Value cpu.A memory)

// Load 8bit value into register
let mvi register value cpu  =
    set8 register value cpu
    |> incPC 2us
    |> incWC 7

// Load value from address in 16bit register to A
let ldax register cpu memory =
    get16 register cpu
    |> fun address -> fetch address.Value memory
    |> fun value -> set8 A value cpu
    |> incPC 1us
    |> incWC 7

// Copy value from A to memory address
let sta (address: DWord) cpu memory =
    incPC 3us cpu 
    |> incWC 13
    |> fun cpu -> cpu, (store address.Value cpu.A memory)

// Copy 8bit value into memory address in HL
let mvi_m value cpu memory =
    let address = get16 HL cpu

    incPC 2us cpu
    |> incWC 10
    |> fun cpu -> cpu, (store address.Value value memory)

// Copy contents of memory address into A
let lda (address: DWord) cpu memory =
    fetch address.Value memory
    |> fun value -> set8 A value cpu
    |> incPC 3us
    |> incWC 13

// Copy 8bit value from register to register
let mov_r_r dest src cpu =
    copy8 src dest cpu
    |> incPC 1us
    |> incWC 5

// Copy 8bit value from address HL to register
let mov_r_m dest cpu memory =
    get16 HL cpu
    |> fun address -> fetch address.Value memory
    |> fun value -> set8 dest value cpu
    |> incPC 1us
    |> incWC 7

// Copy 8bit value from register into address HL
let mov_m_r register cpu memory =
    let address = get16 HL cpu
    let value = get8 register cpu

    incPC 1us cpu
    |> incWC 7
    |> fun cpu -> cpu, (store address.Value value memory)

// Copy value pointed by SP to register and increment SP
let pop register cpu memory =
    let value = {
        High = fetch (cpu.SP + 1us).Value memory;
        Low = fetch cpu.SP.Value memory;
    }

    set16 register value cpu
    |> fun cpu -> { cpu with SP = cpu.SP + 2us }
    |> incPC 1us
    |> incWC 10

// Copy value in register onto stack
let push register cpu memory =
    let value = get16 register cpu
    let memchanges = 
        store (cpu.SP - 2us).Value value.Low memory
        |> store (cpu.SP - 1us).Value value.High

    { cpu with SP = cpu.SP - 2us }
    |> incPC 1us
    |> incWC 11
    |> fun cpu -> (cpu, memchanges)

// Exchange Stack and HL
let xthl cpu memory =
    let hl = get16 HL cpu
    let stack = {
        High = fetch (cpu.SP + 1us).Value memory;
        Low = fetch cpu.SP.Value memory;
    }

    let memchanges = 
        store cpu.SP.Value hl.Low memory
        |> store (cpu.SP + 1us).Value hl.High

    { cpu with H = stack.High; L = stack.Low }
    |> incPC 1us
    |> incWC 18
    |> fun cpu -> (cpu, memchanges)

// Exchange HL and DE
let xchg cpu =
    let hl = get16 HL cpu
    let de = get16 DE cpu

    set16 HL de cpu
    |> set16 DE hl
    |> incPC 1us
    |> incWC 5

// POP the stack back onto A and FLAGS
let pop_psw cpu memory =
    let word = {
        High = fetch (cpu.SP + 1us).Value memory;
        Low = fetch cpu.SP.Value memory;
    }

    { cpu with A = word.High; FLAGS = word.Low; SP = (cpu.SP + 2us) }
    |> incPC 1us
    |> incWC 10

// PUSH A and FLAGS to stack
let push_psw cpu memory =
    let memchanges = 
        store (cpu.SP - 2us).Value cpu.FLAGS memory
        |> store (cpu.SP - 1us).Value cpu.A

    { cpu with SP = cpu.SP - 2us }
    |> incPC 1us
    |> incWC 11
    |> fun cpu -> (cpu, memchanges)