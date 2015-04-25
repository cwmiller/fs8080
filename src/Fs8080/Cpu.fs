module Fs8080.Cpu

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory
open Fs8080.Instructions
open Fs8080.ControlInstructions
open Fs8080.MoveInstructions
open Fs8080.LogicalInstructions
open Fs8080.JumpInstructions

exception UnknownInstruction of byte

// Determines an instruction from its byte value
let decode state memory =
    let opcode = fetch state.PC memory

    let ib = 
        fetch (state.PC+1us) memory

    let iw = 
        let high = fetch (state.PC + 2us) memory
        let low = fetch (state.PC + 1us) memory
        { High = high; Low = low }

    match opcode with
        | 0x00uy -> Instruction.NOP             // No operation
        | 0x01uy -> Instruction.LXI(BC, iw)     // Load 16bit value into BC
        | 0x02uy -> Instruction.STAX(BC)        // Copy 8bit value from A into address BC
        | 0x03uy -> Instruction.INX(BC)         // Increment value in BC by 1
        | 0x04uy -> Instruction.INR(B)          // Increment value in B by 1
        | 0x05uy -> Instruction.DCR(B)          // Decrement value in B by 1
        | 0x06uy -> Instruction.MVI(B, ib)      // Load 8bit value into B
        | 0x07uy -> Instruction.RLC             // Rotate A left
        | 0x08uy -> Instruction.NOP             // Unspecified
        | 0x09uy -> Instruction.DAD(BC)         // Add BC to HL
        | 0x0Auy -> Instruction.LDAX(BC)        // Copy 8bit value from address BC into A
        | 0x0Buy -> Instruction.DCX(BC)         // Decrement value in BC by 1
        | 0x0Cuy -> Instruction.INR(C)          // Increment value in C by 1
        | 0x0Duy -> Instruction.DCR(C)          // Decrement value in C by 1
        | 0x0Euy -> Instruction.MVI(C, ib)      // Load 8bit value into C
        | 0x0Fuy -> Instruction.RRC             // Rotate A right
        | 0x10uy -> Instruction.NOP             // Unspecified
        | 0x11uy -> Instruction.LXI(DE, iw)     // Load 16bit value into DE
        | 0x12uy -> Instruction.STAX(DE)        // Copy 8bit value from A into address DE
        | 0x13uy -> Instruction.INX(DE)         // Increment value in DE by 1
        | 0x14uy -> Instruction.INR(D)          // Increment value in D by 1
        | 0x15uy -> Instruction.DCR(D)          // Decrement value in D by 1
        | 0x16uy -> Instruction.MVI(D, ib)      // Load 8bit value into D
        | 0x17uy -> Instruction.RAL             // Rotate A left through carry
        | 0x18uy -> Instruction.NOP             // Unspecified
        | 0x19uy -> Instruction.DAD(DE)         // Add DE to HL
        | 0x1Auy -> Instruction.LDAX(DE)        // Copy 8bit value from address DE into A
        | 0x1Buy -> Instruction.DCX(DE)         // Decrement value in DE by 1
        | 0x1Cuy -> Instruction.INR(E)          // Increment value in E by 1
        | 0x1Duy -> Instruction.DCR(E)          // Decrement value in E by 1
        | 0x1Euy -> Instruction.MVI(E, ib)      // Load 8bit value into E
        | 0x1Fuy -> Instruction.RAR             // Rotate A right through carry
        | 0x20uy -> Instruction.NOP             // Unspecified
        | 0x21uy -> Instruction.LXI(HL, iw)     // Load 16bit value into HL
        | 0x22uy -> Instruction.SHLD(iw)        // Copy 16bit value from HL into memory address
        | 0x23uy -> Instruction.INX(HL)         // Increment value in HL by 1
        | 0x24uy -> Instruction.INR(H)          // Increment value in H by 1
        | 0x25uy -> Instruction.DCR(H)          // Decrement value in H by 1
        | 0x26uy -> Instruction.MVI(H, ib)      // Load 8but value into H
        | 0x27uy -> Instruction.DAA             // Not sure
        | 0x28uy -> Instruction.NOP             // Unspecified
        | 0x29uy -> Instruction.DAD(HL)         // Add HL to HL
        | 0x2Auy -> Instruction.LHLD(iw)        // Load 16bit value from memory into HL
        | 0x2Buy -> Instruction.DCX(HL)         // Decrement value in HL by 1
        | 0x2Cuy -> Instruction.INR(L)          // Increment value in L by 1
        | 0x2Duy -> Instruction.DCR(L)          // Decrement value in L by 1
        | 0x2Euy -> Instruction.MVI(L, ib)      // Load 8bit value into L
        | 0x2Fuy -> Instruction.CMA             // Set A to NOT A
        | 0x30uy -> Instruction.NOP             // Unspecified
        | 0x31uy -> Instruction.LXI(SP, iw)     // Load 16bit value into SP
        | 0x32uy -> Instruction.STA(iw)         // Copy value from A to memory address
        | 0x33uy -> Instruction.INX(SP)         // Increment value in SP by 1
        | 0x34uy -> Instruction.INR_M           // Increment value in memory address in HL
        | 0x35uy -> Instruction.DCR_M           // Decrement value in memory address in HL
        | 0x36uy -> Instruction.MVI_M(ib)       // Copy 8bit value into the memory address in HL
        | 0x37uy -> Instruction.STC             // Set C flag
        | 0x38uy -> Instruction.NOP             // Unspecified
        | 0x39uy -> Instruction.DAD(SP)         // Add SP to HL
        | 0x3Auy -> Instruction.LDA(iw)         // Copy 8bit value in memory address to A
        | 0x3Buy -> Instruction.DCX(SP)         // Decrement value in SP by 1
        | 0x3Cuy -> Instruction.INR(A)          // Increment value in A by 1
        | 0x3Duy -> Instruction.DCR(A)          // Decrement value in A by 1
        | 0x3Euy -> Instruction.MVI(A, ib)      // Load 8bit value into A
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
        | 0x4Auy -> Instruction.MOV_R_R(C, D)   // Copy 8bit value from D to C
        | 0x4Buy -> Instruction.MOV_R_R(C, E)   // Copy 8bit value from E to C
        | 0x4Cuy -> Instruction.MOV_R_R(C, H)   // Copy 8bit value from H to C
        | 0x4Duy -> Instruction.MOV_R_R(C, L)   // Copy 8bit value from L to C
        | 0x4Euy -> Instruction.MOV_R_M(C)      // Copy 8bit value from address HL to C
        | 0x4Fuy -> Instruction.MOV_R_R(C, A)   // Copy 8bit value from A to C
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
        | 0x5Auy -> Instruction.MOV_R_R(E, D)   // Copy 8bit value from D to E
        | 0x5Buy -> Instruction.MOV_R_R(E, E)   // Copy 8bit value from E to E
        | 0x5Cuy -> Instruction.MOV_R_R(E, H)   // Copy 8bit value from H to E
        | 0x5Duy -> Instruction.MOV_R_R(E, L)   // Copy 8bit value from L to E
        | 0x5Euy -> Instruction.MOV_R_M(E)      // Copy 8bit value from address HL to E
        | 0x5Fuy -> Instruction.MOV_R_R(E, A)   // Copy 8bit value from A to E
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
        | 0x6Auy -> Instruction.MOV_R_R(L, D)   // Copy 8bit value from D to L
        | 0x6Buy -> Instruction.MOV_R_R(L, E)   // Copy 8bit value from E to L
        | 0x6Cuy -> Instruction.MOV_R_R(L, H)   // Copy 8bit value from H to L
        | 0x6Duy -> Instruction.MOV_R_R(L, L)   // Copy 8bit value from L to L
        | 0x6Euy -> Instruction.MOV_R_M(L)      // Copy 8bit value from L to L
        | 0x6Fuy -> Instruction.MOV_R_R(L, A)   // Copy 8bit value from A to L
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
        | 0x7Auy -> Instruction.MOV_R_R(A, D)   // Copy 8bit value from D to A
        | 0x7Buy -> Instruction.MOV_R_R(A, E)   // Copy 8bit value from E to A
        | 0x7Cuy -> Instruction.MOV_R_R(A, H)   // Copy 8bit value from H to A
        | 0x7Duy -> Instruction.MOV_R_R(A, L)   // Copy 8bit value from L to A
        | 0x7Euy -> Instruction.MOV_R_M(A)      // Copy 8bit value from address HL to A
        | 0x7Fuy -> Instruction.MOV_R_R(A, A)   // Copy 8bit value from A to A
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
        | 0x8Fuy -> Instruction.ADC(A)          // Increment A with value in A plus Carry
        | 0x90uy -> Instruction.SUB(B)          // Decrement A with value in B
        | 0x91uy -> Instruction.SUB(C)          // Decrement A with value in C
        | 0x92uy -> Instruction.SUB(D)          // Decrement A with value in D
        | 0x93uy -> Instruction.SUB(E)          // Decrement A with value in E
        | 0x94uy -> Instruction.SUB(H)          // Decrement A with value in H
        | 0x95uy -> Instruction.SUB(L)          // Decrement A with value in L
        | 0x96uy -> Instruction.SUB_M           // Decrement A with value in address in HL
        | 0x97uy -> Instruction.SUB(A)          // Decrement A with value in A
        | 0x98uy -> Instruction.SBB(B)          // Decrement A with value in B with Borrow
        | 0x99uy -> Instruction.SBB(C)          // Decrement A with value in C with Borrow
        | 0x9Auy -> Instruction.SBB(D)          // Decrement A with value in D with Borrow
        | 0x9Buy -> Instruction.SBB(E)          // Decrement A with value in E with Borrow
        | 0x9Cuy -> Instruction.SBB(H)          // Decrement A with value in H with Borrow
        | 0x9Duy -> Instruction.SBB(L)          // Decrement A with value in L with Borrow
        | 0x9Euy -> Instruction.SBB_M           // Decrement A with value in address in HL with Borrow
        | 0x9Fuy -> Instruction.SBB(A)          // Decrement A with value in A with Borrow

        | 0xC0uy -> Instruction.RNZ             // RET if Z flag is not set
        | 0xC1uy -> Instruction.POP(BC)         // POP stack to BC
        | 0xC2uy -> Instruction.JNZ(iw)         // Jump to memory address if Z flag is not set
        | 0xC3uy -> Instruction.JMP(iw)         // Jump to memory address
        | 0xC4uy -> Instruction.CNZ(iw)         // CALL memory address if Z flag is not set
        | 0xC5uy -> Instruction.PUSH(BC)        // PUSH stack with value in BC


        | 0xC8uy -> Instruction.RZ              // RET if Z flag is set
        | 0xC9uy -> Instruction.RET             // Pop stack to PC and jump back
        | 0xCAuy -> Instruction.JZ(iw)          // Jump to memory address if Z flag is set
        | 0xCBuy -> Instruction.JMP(iw)         // Alernate for JMP (but shouldn't be used)
        | 0xCCuy -> Instruction.CZ(iw)          // CALL memory address if Z flag is set
        | 0xCDuy -> Instruction.CALL(iw)        // Push PC to stack and jump to address

        | 0xD0uy -> Instruction.RNC             // RET if C flag not set
        | 0xD1uy -> Instruction.POP(DE)         // POP stack to DE
        | 0xD2uy -> Instruction.JNC(iw)         // Jump to memory address if C flag is not set

        | 0xD4uy -> Instruction.CNC(iw)         // CALL memory address if C flag is not set
        | 0xD5uy -> Instruction.PUSH(DE)        // PUSH stack with value in DE
        
        | 0xDDuy -> Instruction.CALL(iw)        // Alternative for CALL (but shouldn't be used)

        | 0xE1uy -> Instruction.POP(HL)         // POP stack to HL

        | 0xE5uy -> Instruction.PUSH(HL)        // PUSH stack with value in H
        | 0xE6uy -> Instruction.ANI             // AND A against 8bit value

        | 0xEDuy -> Instruction.CALL(iw)        // Alternative for CALL (but shouldn't be used)

        | 0xFDuy -> Instruction.CALL(iw)        // Alternative for CALL (but shouldn't be used)

        | _ -> raise (UnknownInstruction(opcode))

// Carries out the given instruction
// Returns the post-execution state along with a list of changes to perform on memory
let execute instruction state memory =
    match instruction with
        | NOP                   -> nop state, []
        | LXI(reg, value)       -> lxi reg value state, []
        | STAX(reg)             -> stax reg state
        | INX(reg)              -> inx reg state, []
        | INR(reg)              -> inr reg state, []
        | DCR(reg)              -> dcr reg state, []
        | MVI(reg, value)       -> mvi reg value state, []
        | RLC                   -> rlc state, []
        | DAD(reg)              -> dad reg state, []
        | LDAX(reg)             -> ldax reg state memory, []
        | DCX(reg)              -> dcx reg state, []
        | RRC                   -> rrc state, []
        | RAL                   -> ral state, []
        | RAR                   -> rar state, []
        | SHLD(address)         -> shld address state
        | DAA                   -> daa state, []
        | LHLD(address)         -> lhld address state memory, []
        | CMA                   -> cma state, []
        | STA(address)          -> sta address state
        | INR_M                 -> inr_m state memory
        | DCR_M                 -> dcr_m state memory
        | MVI_M(value)          -> mvi_m value state
        | STC                   -> stc state, []
        | LDA(address)          -> lda address state memory, []
        | CMC                   -> cmc state, []
        | MOV_R_R(dest, src)    -> mov_r_r dest src state, []
        | MOV_R_M(reg)          -> mov_r_m reg state memory, []
        | MOV_M_R(reg)          -> mov_m_r reg state
        | HLT                   -> hlt state, []
        | ADD(reg)              -> add reg state, []
        | ADD_M                 -> add_m state memory, []
        | ADC(reg)              -> adc reg state, []
        | ADC_M                 -> adc_m state memory, []
        | SUB(reg)              -> sub reg state, []
        | SUB_M                 -> sub_m state memory, []
        | SBB(reg)              -> sbb reg state, []
        | SBB_M                 -> sbb_m state memory, []
        | RNZ                   -> rnz state memory, []
        | POP(reg)              -> pop reg state memory, []
        | JNZ(address)          -> jnz address state, []
        | JMP(address)          -> jmp address state, []
        | CNZ(address)          -> cnz address state
        | PUSH(reg)             -> push reg state
        | RZ                    -> rz state memory, []
        | RET                   -> ret state memory, []
        | JZ(address)           -> jz address state, []
        | CZ(address)           -> cz address state
        | CALL(address)         -> call address state 
        | RNC                   -> rnc state memory, []
        | JNC(address)          -> jnc address state, []
        | CNC(address)          -> cnc address state
        | ANI                   -> ani state memory, []

// fetch-decode-execute cycle! Where all the magic begins.
let rec cycle state memory =
    decode state memory
    |> fun(instruction) -> execute instruction state memory
    |> fun (state, changes) ->
        List.iter (fun (addr, value) -> store addr value memory) changes
        state
    |> fun state -> cycle state memory