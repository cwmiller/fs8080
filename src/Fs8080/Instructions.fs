﻿module Fs8080.Instructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

// List of supported instructions
type Instruction =
    | NOP
    | LXI of Register16 * DWord
    | STAX of Register16
    | INX of Register16
    | INR of Register8
    | DCR of Register8
    | MVI of Register8 * byte
    | RLC
    | DAD of Register16
    | LDAX of Register16
    | DCX of Register16
    | RRC
    | RAL
    | RAR
    | SHLD of DWord
    | DAA
    | LHLD of DWord
    | CMA
    | STA of DWord
    | INR_M
    | DCR_M
    | MVI_M of byte
    | STC
    | LDA of DWord
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
    | ANA of Register8
    | ANA_M
    | XRA of Register8
    | XRA_M
    | ORA of Register8
    | ORA_M
    | CMP of Register8
    | CMP_M
    | RNZ
    | POP of Register16
    | JNZ of DWord
    | JMP of DWord
    | CNZ of DWord
    | PUSH of Register16
    | ADI of byte
    | RZ
    | RET
    | JZ of DWord
    | CZ of DWord
    | CALL of DWord
    | ACI of byte 
    | RNC
    | JNC of DWord
    | CNC of DWord
    | SUI of byte
    | RC
    | JC of DWord
    | CC of DWord
    | SBI of byte
    | RPO
    | JPO of DWord
    | XTHL
    | CPO of DWord
    | ANI of byte
    | RPE
    | JPE of DWord
    | XCHG
    | CPE of DWord
    | XRI of byte
    | RP
    | JP of DWord
    | CP of DWord
    | ORI of byte
    | RM
    | JM of DWord
    | CM of DWord
    | CPI of byte