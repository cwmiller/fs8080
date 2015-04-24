module Fs8080.LogicalInstructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

// Increment value in 16bit register 
let inx register state =
    (get16 register state) + 1us
    |> fun value -> set16 register value state
    |> incPC 1us
    |> incWC 5

// Increment value in 8bit register
let inr register state =
    let value = (get8 register state) + 1uy
            
    set8 register value state
    |> flagSZAP value
    |> incPC 1us
    |> incWC 5

// Decrement value in 8bit register
let dcr register state =
    let value = (get8 register state) - 1uy

    set8 register value state
    |> flagSZAP value
    |> incPC 1us
    |> incWC 5

// Not sure what DAA is.
// According to http://pastraiser.com/cpu/i8080/i8080_opcodes.html it alters all FLAGS,
// but I can't find any details on it.
let daa state =
    incPC 1us state
    |> incWC 4

// Rotate A left
let rlc state = 
    let value = get8 A state
    let highbit = value >>> 7

    set8 A (value <<< 1) state
    |> fun state -> 
        if highbit = 1uy // Set C flag to previous high bit
        then { state with FLAGS = state.FLAGS ||| FlagMask.C; }
        else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4     

// Add value in 16bit register to HL
let dad register state =
    let existing = get16 HL state
    let sum = (get16 register state) + existing

    set16 HL sum state
    |> fun state ->
        if sum < existing // Set Carry flag if new value overflowed
        then { state with FLAGS = state.FLAGS ||| FlagMask.C; }         
        else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 10

// Decrement 16bit register
let dcx register state =
    get16 register state
    |> fun value -> set16 register (value - 1us) state
    |> incPC 1us
    |> incWC 5

// Rotate A right
let rrc state =
    let value = get8 A state
    let lowbit = value &&& 0x01uy
    // Shift the value right and set the high bit to the old low bit
    let shifted = (value >>> 1) ||| (lowbit <<< 7)

    set8 A shifted state
    |> fun state -> 
        if lowbit = 1uy // Set C flag to previous low bit
        then { state with FLAGS = state.FLAGS ||| FlagMask.C; }
        else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4  

// Rotate A left through carry
let ral state =
    let value = get8 A state
    let highbit = value >>> 7

    // Shift the value left and set the lowbit to the C flag
    let shifted = (value <<< 1) ||| (state.FLAGS &&& FlagMask.C)

    set8 A shifted state
    |> fun state -> 
        if highbit = 1uy // Set C flag to previous high bit
        then { state with FLAGS = state.FLAGS ||| FlagMask.C; }
        else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4   

// Rotate A right through carry
let rar state =
    let value = get8 A state
    let highbit = value >>> 7
    let lowbit = value &&& 0x01uy

    // Shift the value right and set the high bit to the previous high bit
    let adjusted = (value >>> 1) ||| (highbit <<< 7)

    set8 A adjusted state
    |> fun state -> 
        if lowbit = 1uy // Set C flag to previous low bit
        then { state with FLAGS = state.FLAGS ||| FlagMask.C; }
        else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4  

// Set A to NOT A
let cma state =
    { state with A = ~~~state.A; }
    |> incPC 1us
    |> incWC 4

// Increment value in memory pointed to by HL 
let inr_m state memory =
    let address = (get16 HL state)
    let value = (fetch address memory) + 1uy
            
    flagSZAP value state
    |> incPC 1us
    |> incWC 10
    |> fun state -> (state, [(address, value)])


// Decrement value in memory pointed to by HL 
let dcr_m state memory =
    let address = (get16 HL state)
    let value = (fetch address memory) - 1uy
            
    flagSZAP value state
    |> incPC 1us
    |> incWC 10
    |> fun state -> (state, [(address, value)])

// Enables the C flag
let stc state =
    { state with FLAGS = state.FLAGS ||| FlagMask.C }
    |> incPC 1us
    |> incWC 4

// Set the C flag to NOT C
let cmc state =
    { state with FLAGS = state.FLAGS ^^^ FlagMask.C }
    |> incPC 1us
    |> incWC 4

// Increment A by 8bit register
let add register state =
    let existing = get8 A state
    let sum = existing + (get8 register state)

    set8 A sum state
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 4

// Increment A by value in address in HL
let add_m state memory =
    let value = fetch (get16 HL state) memory
    let existing = get8 A state
    let sum = existing + value

    set8 A sum state
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 7

// Increment A by register and Carry
let adc register state =
    let existing = get8 A state
    let sum = existing + (get8 register state) + (state.FLAGS &&& FlagMask.C)

    set8 A sum state
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 4

// Increment A by value in address in HL and Carry
let adc_m state memory =
    let value = fetch (get16 HL state) memory
    let existing = get8 A state
    let sum = existing + value + (state.FLAGS &&& FlagMask.C)

    set8 A sum state
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 7

// Decrement A by value in register
let sub register state =
    let existing = get8 A state
    let diff = existing - (get8 register state)

    set8 A diff state
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 4

// Decrement A by value pointed to by HL
let sub_m state memory =
    let existing = get8 A state

    let diff =
        get16 HL state
        |> fun addr -> fetch addr memory
        |> (-) existing

    set8 A diff state
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 7

// Decrement A by value in register with carry
let sbb register state =
    let existing = get8 A state
    let diff = existing - (get8 register state) - (state.FLAGS &&& FlagMask.C)

    set8 A diff state
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 4

// Decrement A by value in memory pointed to by HL and Carry
let sbb_m state memory =
    let existing = get8 A state

    let diff =
        get16 HL state
        |> fun addr -> fetch addr memory
        |> (-) (existing - (state.FLAGS &&& FlagMask.C))

    set8 A diff state
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 7

// AND 8bit value against A
let ani state memory =
    let value = 
        fetch (state.PC + 1us) memory
        |> (&&&) state.A

    set8 A value state
    |> flagSZAP value
    |> incPC 2us
    |> incWC 7