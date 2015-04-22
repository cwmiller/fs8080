module Fs8080.Instructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

exception UnknownInstruction of byte

// List of supported instructions
type Instruction =
    | NOP
    | LXI of Register16
    | STAX of Register16
    | INX of Register16
    | INR of Register8
    | DCR of Register8
    | MVI of Register8
    | RLC
    | DAD of Register16
    | LDAX of Register16
    | DCX of Register16
    | RRC
    | RAL
    | RAR
    | SHLD
    | DAA
    | LHLD
    | CMA
    | STA
    | INR_M
    | DCR_M
    | MVI_M
    | STC
    | LDA
    | CMC
    | MOV_R_R of Register8 * Register8
    | MOV_R_M of Register8
    | MOV_M_R of Register8
    | HLT
    | ADD of Register8
    | ADD_M
    | ADC of Register8
    | ADC_M

// Determines an instruction from its byte value
let decode byte =
    match byte with
        | 0x00uy -> Instruction.NOP             // No operation
        | 0x01uy -> Instruction.LXI(BC)         // Load 16bit value into BC
        | 0x02uy -> Instruction.STAX(BC)        // Copy 8bit value from A into address BC
        | 0x03uy -> Instruction.INX(BC)         // Increment value in BC by 1
        | 0x04uy -> Instruction.INR(B)          // Increment value in B by 1
        | 0x05uy -> Instruction.DCR(B)          // Decrement value in B by 1
        | 0x06uy -> Instruction.MVI(B)          // Load 8bit value into B
        | 0x07uy -> Instruction.RLC             // Rotate A left
        | 0x08uy -> Instruction.NOP             // Unspecified
        | 0x09uy -> Instruction.DAD(BC)         // Add BC to HL
        | 0x0Auy -> Instruction.LDAX(BC)        // Copy 8bit value from address BC into A
        | 0x0Buy -> Instruction.DCX(BC)         // Decrement value in BC by 1
        | 0x0Cuy -> Instruction.INR(C)          // Increment value in C by 1
        | 0x0Duy -> Instruction.DCR(C)          // Decrement value in C by 1
        | 0x0Euy -> Instruction.MVI(C)          // Load 8bit value into C
        | 0x0Fuy -> Instruction.RRC             // Rotate A right
        | 0x10uy -> Instruction.NOP             // Unspecified
        | 0x11uy -> Instruction.LXI(DE)         // Load 16bit value into DE
        | 0x12uy -> Instruction.STAX(DE)        // Copy 8bit value from A into address DE
        | 0x13uy -> Instruction.INX(DE)         // Increment value in DE by 1
        | 0x14uy -> Instruction.INR(D)          // Increment value in D by 1
        | 0x15uy -> Instruction.DCR(D)          // Decrement value in D by 1
        | 0x16uy -> Instruction.MVI(D)          // Load 8bit value into D
        | 0x17uy -> Instruction.RAL             // Rotate A left through carry
        | 0x18uy -> Instruction.NOP             // Unspecified
        | 0x19uy -> Instruction.DAD(DE)         // Add DE to HL
        | 0x1Auy -> Instruction.LDAX(DE)        // Copy 8bit value from address DE into A
        | 0x1Buy -> Instruction.DCX(DE)         // Decrement value in DE by 1
        | 0x1Cuy -> Instruction.INR(E)          // Increment value in E by 1
        | 0x1Duy -> Instruction.DCR(E)          // Decrement value in E by 1
        | 0x1Euy -> Instruction.MVI(E)          // Load 8bit value into E
        | 0x1Fuy -> Instruction.RAR             // Rotate A right through carry
        | 0x20uy -> Instruction.NOP             // Unspecified
        | 0x21uy -> Instruction.LXI(HL)         // Load 16bit value into HL
        | 0x22uy -> Instruction.SHLD            // Copy 16bit value from HL into memory address
        | 0x23uy -> Instruction.INX(HL)         // Increment value in HL by 1
        | 0x24uy -> Instruction.INR(H)          // Increment value in H by 1
        | 0x25uy -> Instruction.DCR(H)          // Decrement value in H by 1
        | 0x26uy -> Instruction.MVI(H)          // Load 8but value into H
        | 0x27uy -> Instruction.DAA             // Not sure
        | 0x28uy -> Instruction.NOP             // Unspecified
        | 0x29uy -> Instruction.DAD(HL)         // Add HL to HL
        | 0x2auy -> Instruction.LHLD            // Load 16bit value from memory into HL
        | 0x2buy -> Instruction.DCX(HL)         // Decrement value in HL by 1
        | 0x2cuy -> Instruction.INR(L)          // Increment value in L by 1
        | 0x2duy -> Instruction.DCR(L)          // Decrement value in L by 1
        | 0x2euy -> Instruction.MVI(L)          // Load 8bit value into L
        | 0x2fuy -> Instruction.CMA             // Set A to NOT A
        | 0x30uy -> Instruction.NOP             // Unspecified
        | 0x31uy -> Instruction.LXI(SP)         // Load 16bit value into SP
        | 0x32uy -> Instruction.STA             // Copy value from A to memory address
        | 0x33uy -> Instruction.INX(SP)         // Increment value in SP by 1
        | 0x34uy -> Instruction.INR_M           // Increment value in memory address in HL
        | 0x35uy -> Instruction.DCR_M           // Decrement value in memory address in HL
        | 0x36uy -> Instruction.MVI_M           // Copy 8bit value into the memory address in HL
        | 0x37uy -> Instruction.STC             // Set C flag
        | 0x38uy -> Instruction.NOP             // Unspecified
        | 0x39uy -> Instruction.DAD(SP)         // Add SP to HL
        | 0x3Auy -> Instruction.LDA             // Copy 8bit value in memory address to A
        | 0x3Buy -> Instruction.DCX(SP)         // Decrement value in SP by 1
        | 0x3Cuy -> Instruction.INR(A)          // Increment value in A by 1
        | 0x3Duy -> Instruction.DCR(A)          // Decrement value in A by 1
        | 0x3Euy -> Instruction.MVI(A)          // Load 8bit value into A
        | 0x3Fuy -> Instruction.CMC             // NOT the C flag
        | 0x40uy -> Instruction.MOV_R_R(B, B)   // Copy 8bit value from B to B
        | 0x41uy -> Instruction.MOV_R_R(B, C)   // Copy 8bit value from C to B
        | 0x42uy -> Instruction.MOV_R_R(B, D)   // Copy 8bit value from D to B
        | 0x43uy -> Instruction.MOV_R_R(B, E)   // Copy 8bit value from E to B
        | 0x44uy -> Instruction.MOV_R_R(B, H)   // Copy 8bit value from H to B
        | 0x45uy -> Instruction.MOV_R_R(B, L)   // Copy 8bit value from L to B
        | 0x46uy -> Instruction.MOV_R_M(B)      // Copy 8bit value from address HL to B
        | 0x47uy -> Instruction.MOV_R_R(B, A)   // Copy 8bit value from A to B
        | 0x48uy -> Instruction.MOV_R_R(C, B)   // Copy 8bit value from B to C
        | 0x49uy -> Instruction.MOV_R_R(C, C)   // Copy 8bit value from C to C
        | 0x4auy -> Instruction.MOV_R_R(C, D)   // Copy 8bit value from D to C
        | 0x4buy -> Instruction.MOV_R_R(C, E)   // Copy 8bit value from E to C
        | 0x4cuy -> Instruction.MOV_R_R(C, H)   // Copy 8bit value from H to C
        | 0x4duy -> Instruction.MOV_R_R(C, L)   // Copy 8bit value from L to C
        | 0x4euy -> Instruction.MOV_R_M(C)      // Copy 8bit value from address HL to C
        | 0x4fuy -> Instruction.MOV_R_R(C, A)   // Copy 8bit value from A to C
        | 0x50uy -> Instruction.MOV_R_R(D, B)   // Copy 8bit value from B to D
        | 0x51uy -> Instruction.MOV_R_R(D, C)   // Copy 8bit value from C to D
        | 0x52uy -> Instruction.MOV_R_R(D, D)   // Copy 8bit value from D to D
        | 0x53uy -> Instruction.MOV_R_R(D, E)   // Copy 8bit value from E to D
        | 0x54uy -> Instruction.MOV_R_R(D, H)   // Copy 8bit value from H to D
        | 0x55uy -> Instruction.MOV_R_R(D, L)   // Copy 8bit value from L to D
        | 0x56uy -> Instruction.MOV_R_M(D)      // Copy 8bit value from address HL to D
        | 0x57uy -> Instruction.MOV_R_R(D, A)   // Copy 8bit value from A to D
        | 0x58uy -> Instruction.MOV_R_R(E, B)   // Copy 8bit value from B to E
        | 0x59uy -> Instruction.MOV_R_R(E, C)   // Copy 8bit value from C to E
        | 0x5auy -> Instruction.MOV_R_R(E, D)   // Copy 8bit value from D to E
        | 0x5buy -> Instruction.MOV_R_R(E, E)   // Copy 8bit value from E to E
        | 0x5cuy -> Instruction.MOV_R_R(E, H)   // Copy 8bit value from H to E
        | 0x5duy -> Instruction.MOV_R_R(E, L)   // Copy 8bit value from L to E
        | 0x5euy -> Instruction.MOV_R_M(E)      // Copy 8bit value from address HL to E
        | 0x5fuy -> Instruction.MOV_R_R(E, A)   // Copy 8bit value from A to E
        | 0x60uy -> Instruction.MOV_R_R(H, B)   // Copy 8bit value from B to H
        | 0x61uy -> Instruction.MOV_R_R(H, C)   // Copy 8bit value from c to H
        | 0x62uy -> Instruction.MOV_R_R(H, D)   // Copy 8bit value from D to H
        | 0x63uy -> Instruction.MOV_R_R(H, E)   // Copy 8bit value from E to H
        | 0x64uy -> Instruction.MOV_R_R(H, H)   // Copy 8bit value from H to H
        | 0x65uy -> Instruction.MOV_R_R(H, L)   // Copy 8bit value from L to H
        | 0x66uy -> Instruction.MOV_R_M(H)      // Copy 8bit value from address HL to H
        | 0x67uy -> Instruction.MOV_R_R(H, A)   // Copy 8bit value from A to H
        | 0x68uy -> Instruction.MOV_R_R(L, B)   // Copy 8bit value from B to L
        | 0x69uy -> Instruction.MOV_R_R(L, C)   // Copy 8bit value from C to L
        | 0x6auy -> Instruction.MOV_R_R(L, D)   // Copy 8bit value from D to L
        | 0x6buy -> Instruction.MOV_R_R(L, E)   // Copy 8bit value from E to L
        | 0x6cuy -> Instruction.MOV_R_R(L, H)   // Copy 8bit value from H to L
        | 0x6duy -> Instruction.MOV_R_R(L, L)   // Copy 8bit value from L to L
        | 0x6euy -> Instruction.MOV_R_M(L)      // Copy 8bit value from L to L
        | 0x6fuy -> Instruction.MOV_R_R(L, A)   // Copy 8bit value from A to L
        | 0x70uy -> Instruction.MOV_M_R(B)      // Copy 8bit value from B to address HL
        | 0x71uy -> Instruction.MOV_M_R(C)      // Copy 8bit value from C to address HL
        | 0x72uy -> Instruction.MOV_M_R(D)      // Copy 8bit value from D to address HL
        | 0x73uy -> Instruction.MOV_M_R(E)      // Copy 8bit value from E to address HL
        | 0x74uy -> Instruction.MOV_M_R(H)      // Copy 8bit value from H to address HL
        | 0x75uy -> Instruction.MOV_M_R(L)      // Copy 8bit value from L to address HL
        | 0x76uy -> Instruction.HLT             // Halt CPU
        | 0x77uy -> Instruction.MOV_M_R(A)      // Copy 8bit value in A to address HL
        | 0x78uy -> Instruction.MOV_R_R(A, B)   // Copy 8bit value from B to A
        | 0x79uy -> Instruction.MOV_R_R(A, C)   // Copy 8bit value from C to A
        | 0x7auy -> Instruction.MOV_R_R(A, D)   // Copy 8bit value from D to A
        | 0x7buy -> Instruction.MOV_R_R(A, E)   // Copy 8bit value from E to A
        | 0x7cuy -> Instruction.MOV_R_R(A, H)   // Copy 8bit value from H to A
        | 0x7duy -> Instruction.MOV_R_R(A, L)   // Copy 8bit value from L to A
        | 0x7euy -> Instruction.MOV_R_M(A)      // Copy 8bit value from address HL to A
        | 0x7fuy -> Instruction.MOV_R_R(A, A)   // Copy 8bit value from A to A
        | 0x80uy -> Instruction.ADD(B)          // Increment A with value in B
        | 0x81uy -> Instruction.ADD(C)          // Increment A with value in C
        | 0x82uy -> Instruction.ADD(D)          // Increment A with value in D
        | 0x83uy -> Instruction.ADD(D)          // Increment A with value in D
        | 0x84uy -> Instruction.ADD(E)          // Increment A with value in E
        | 0x85uy -> Instruction.ADD(H)          // Increment A with value in H
        | 0x86uy -> Instruction.ADD(L)          // Increment A with value in L
        | 0x87uy -> Instruction.ADD_M           // Increment A with value in address in HL
        | 0x88uy -> Instruction.ADC(B)          // Increment A with value in B plus Carry
        | 0x89uy -> Instruction.ADC(C)          // Increment A with value in C plus Carry
        | 0x8Auy -> Instruction.ADC(D)          // Increment A with value in D plus Carry
        | 0x8Buy -> Instruction.ADC(E)          // Increment A with value in E plus Carry
        | 0x8Cuy -> Instruction.ADC(H)          // Increment A with value in H plus Carry
        | 0x8Duy -> Instruction.ADC(L)          // Increment A with value in L plus Carry
        | 0x8Euy -> Instruction.ADC_M           // Increment A with value in address in HL plus Carry

        | _ -> raise (UnknownInstruction(byte))


(*
 * Handlers for all the instructions
 *)


// No operation
let nop state =
     incPC 1us state
     |> incWC 4


// Loads 16 bit value into register BC, DE, HL, or SP
let lxi register state memory =
    let address = {
        High = fetch (state.PC + 2us) memory;
        Low = fetch (state.PC + 1us) memory;
    }

    set16 register address state
    |> incPC 3us
    |> incWC 10


// Copy 8bit value from A into address in BC or DE
let stax register state =
    let value = get8 A state
    let address = get16 register state
    (incPC 1us state |> incWC 7, [(address, value)])

   
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
    |> setSZAP value
    |> incPC 1us
    |> incWC 5


// Decrement value in 8bit register
let dcr register state =
    let value = (get8 register state) - 1uy

    set8 register value state
    |> setSZAP value
    |> incPC 1us
    |> incWC 5


// Load 8bit value into register
let mvi register state memory =
    set8 register (fetch (state.PC + 1us) memory) state
    |> incPC 2us
    |> incWC 7


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


// Load value from address in 16bit register to A
let ldax register state memory =
    get16 register state
    |> fun address -> fetch address memory
    |> fun value -> set8 A value state
    |> incPC 1us
    |> incWC 7


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


// Not sure what DAA is.
// According to http://pastraiser.com/cpu/i8080/i8080_opcodes.html it alters all FLAGS,
// but I can't find any details on it.
let daa state =
    incPC 1us state
    |> incWC 4

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


// Set A to NOT A
let cma state =
    { state with A = ~~~state.A; }
    |> incPC 1us
    |> incWC 4

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


// Increment value in memory pointed to by HL 
let inr_m state memory =
    let address = (get16 HL state)
    let value = (fetch address memory) + 1uy
            
    setSZAP value state
    |> incPC 1us
    |> incWC 10
    |> fun state -> (state, [(address, value)])


// Decrement value in memory pointed to by HL 
let dcr_m state memory =
    let address = (get16 HL state)
    let value = (fetch address memory) - 1uy
            
    setSZAP value state
    |> incPC 1us
    |> incWC 10
    |> fun state -> (state, [(address, value)])
    

// Copy 8bit value into memory address in HL
let mvi_m state memory =
    let address = get16 HL state
    let value = fetch (state.PC + 1us) memory

    incPC 2us state
    |> incWC 10
    |> fun state -> (state, [(address, value)])


// Enables the C flag
let stc state =
    { state with FLAGS = state.FLAGS ||| FlagMask.C }
    |> incPC 1us
    |> incWC 4


// Copy contents of memory address into A
let lda state memory =
    let address = {
        High = fetch (state.PC + 2us) memory;
        Low = fetch (state.PC + 1us) memory;
    }

    set8 A (fetch address memory) state
    |> incPC 3us
    |> incWC 13


// Set the C flag to NOT C
let cmc state =
    { state with FLAGS = state.FLAGS ^^^ FlagMask.C }
    |> incPC 1us
    |> incWC 4

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

   
// Halt CPU
let hlt state =
    incPC 1us state
    |> incWC 7

// Increment A by 8bit register
let add register state =
    let existing = get8 A state
    let sum = existing + (get8 register state)

    set8 A sum state
    |> setSZAP sum
    |> flagC existing sum
    |> incPC 1us
    |> incWC 4


// Increment A by value in address in HL
let add_m state memory =
    let value = fetch (get16 HL state) memory
    let existing = get8 A state
    let sum = existing + value

    set8 A sum state
    |> setSZAP sum
    |> flagC existing sum
    |> incPC 1us
    |> incWC 7

// Increment A by register and Carry
let adc register state =
    let existing = get8 A state
    let sum = existing + (get8 register state) + (state.FLAGS &&& FlagMask.C)

    set8 A sum state
    |> setSZAP sum
    |> flagC existing sum
    |> incPC 1us
    |> incWC 4

// Increment A by value in address in HL and Carry
let adc_m state memory =
    let value = fetch (get16 HL state) memory
    let existing = get8 A state
    let sum = existing + value + (state.FLAGS &&& FlagMask.C)

    set8 A sum state
    |> setSZAP sum
    |> flagC existing sum
    |> incPC 1us
    |> incWC 7