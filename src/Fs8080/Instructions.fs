module Fs8080.Instructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

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
    | SUB of Register8
    | SUB_M
    | SBB of Register8
    | SBB_M
    | RNZ
    | POP of Register16
    | JNZ
    | JMP
    | CNZ
    | PUSH of Register16
    | RZ
    | RET
    | JZ
    | CZ
    | CALL
    | RNC
    | JNC
    | CNC
    | ANI

let immediateByte state memory =
    fetch (state.PC + 1us) memory

let immediateWord state memory = {
    High = fetch (state.PC + 2us) memory;
    Low = fetch (state.PC + 1us) memory;
}