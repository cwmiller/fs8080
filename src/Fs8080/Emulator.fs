﻿namespace Fs8080

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory
open Fs8080.ControlInstructions
open Fs8080.MoveInstructions
open Fs8080.LogicalInstructions
open Fs8080.JumpInstructions

open System.Diagnostics

exception UnknownInstruction of byte
exception UnknownDevice of byte

type Emulator() =
    let defaultCpu = {
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
        INTE = true;
        State = Running;
    }

    let mutable stopping = false

    let mutable currentCpu = defaultCpu

    let mutable interruptRequests = List.empty<Instruction>

    let mutable inputDevices = Map.empty<byte, InputDevice>

    let mutable outputDevices = Map.empty<byte, OutputDevice>

    let watch = new Stopwatch()

    let frequencyNano = (float)(Stopwatch.Frequency / 1000L)

    let cycleEvent = new Event<_>()

    let executionEvent = new Event<_>()

    let postExecutionEvent = new Event<_>()

    // Event that fires every CPU cycle
    [<CLIEvent>]
    member this.OnCycle = cycleEvent.Publish

    // Event that fires prior to instruction being executed
    [<CLIEvent>]
    member this.OnExecution = executionEvent.Publish

    // Event that fires following an instruction being executed
    [<CLIEvent>]
    member this.AfterExecution = postExecutionEvent.Publish

    // CPU clock speed
    member this.Hz = 2000000.0

    // Determines the next instruction in memory
    member private this.FetchAndDecode (cpu, memory) =
        let opcode = fetch cpu.PC.Value memory

        // Gets the immediate bye following the instruction
        let ib = 
            fetch (cpu.PC+1us).Value memory

        // Gets the immediate word following the instruction
        let iw = 
            let high = fetch (cpu.PC + 2us).Value memory
            let low = fetch (cpu.PC + 1us).Value memory
            { High = high; Low = low }

        match opcode with
            | 0x00uy -> Instruction.NOP                 // No operation
            | 0x01uy -> Instruction.LXI(RegBC, iw)      // BC = Word
            | 0x02uy -> Instruction.STAX(RegBC)         // (BC) = A
            | 0x03uy -> Instruction.INX(RegBC)          // BC = BC + 1
            | 0x04uy -> Instruction.INR(RegB)           // B = B + 1
            | 0x05uy -> Instruction.DCR(RegB)           // B = B - 1
            | 0x06uy -> Instruction.MVI(RegB, ib)       // B = byte
            | 0x07uy -> Instruction.RLC                 // A = A << 1
            | 0x08uy -> Instruction.NOP                 // Alternative for NOP (do not use)
            | 0x09uy -> Instruction.DAD(RegBC)          // HL = HL + BC
            | 0x0Auy -> Instruction.LDAX(RegBC)         // A = (BC)
            | 0x0Buy -> Instruction.DCX(RegBC)          // BC = BC - 1
            | 0x0Cuy -> Instruction.INR(RegC)           // C = C + 1
            | 0x0Duy -> Instruction.DCR(RegC)           // C = C - 1
            | 0x0Euy -> Instruction.MVI(RegC, ib)       // C = byte
            | 0x0Fuy -> Instruction.RRC                 // A = A >> 1
            | 0x10uy -> Instruction.NOP                 // Alternative for NOP (do not use)
            | 0x11uy -> Instruction.LXI(RegDE, iw)      // DE = word
            | 0x12uy -> Instruction.STAX(RegDE)         // (DE) = A
            | 0x13uy -> Instruction.INX(RegDE)          // DE = DE + 1
            | 0x14uy -> Instruction.INR(RegD)           // D = D + 1
            | 0x15uy -> Instruction.DCR(RegD)           // D = D - 1
            | 0x16uy -> Instruction.MVI(RegD, ib)       // D = byte
            | 0x17uy -> Instruction.RAL                 // A << 1 through carry
            | 0x18uy -> Instruction.NOP                 // Alternative for NOP (do not use)
            | 0x19uy -> Instruction.DAD(RegDE)          // HL = HL + DE
            | 0x1Auy -> Instruction.LDAX(RegDE)         // A = (DE)
            | 0x1Buy -> Instruction.DCX(RegDE)          // DE = DE - 1
            | 0x1Cuy -> Instruction.INR(RegE)           // E = E + 1
            | 0x1Duy -> Instruction.DCR(RegE)           // E = E - 1
            | 0x1Euy -> Instruction.MVI(RegE, ib)       // E = byte
            | 0x1Fuy -> Instruction.RAR                 // A = A >> 1 through carry
            | 0x20uy -> Instruction.NOP                 // Alternative for NOP (do not use)
            | 0x21uy -> Instruction.LXI(RegHL, iw)      // HL = word
            | 0x22uy -> Instruction.SHLD(iw)            // (word) = HL
            | 0x23uy -> Instruction.INX(RegHL)          // HL = HL + 1
            | 0x24uy -> Instruction.INR(RegH)           // H = H + 1
            | 0x25uy -> Instruction.DCR(RegH)           // H = H - 1
            | 0x26uy -> Instruction.MVI(RegH, ib)       // H = byte
            | 0x27uy -> Instruction.DAA                 // Decimal Adjust Accumulator
            | 0x28uy -> Instruction.NOP                 // Alternative for NOP (do not use)
            | 0x29uy -> Instruction.DAD(RegHL)          // HL = HL + HL
            | 0x2Auy -> Instruction.LHLD(iw)            // HL = (word)
            | 0x2Buy -> Instruction.DCX(RegHL)          // HL = HL - 1
            | 0x2Cuy -> Instruction.INR(RegL)           // L = L + 1
            | 0x2Duy -> Instruction.DCR(RegL)           // L = L - 1
            | 0x2Euy -> Instruction.MVI(RegL, ib)       // L = byte
            | 0x2Fuy -> Instruction.CMA                 // A = NOT A
            | 0x30uy -> Instruction.NOP                 // Alternative for NOP (do not use)
            | 0x31uy -> Instruction.LXI(RegSP, iw)      // SP = word
            | 0x32uy -> Instruction.STA(iw)             // (word) = A
            | 0x33uy -> Instruction.INX(RegSP)          // SP = SP + 1
            | 0x34uy -> Instruction.INR_M               // (HL) = (HL) + 1
            | 0x35uy -> Instruction.DCR_M               // (HL) = (HL) - 1
            | 0x36uy -> Instruction.MVI_M(ib)           // (HL) = byte
            | 0x37uy -> Instruction.STC                 // Set the Carry flag
            | 0x38uy -> Instruction.NOP                 // Alternative for NOP
            | 0x39uy -> Instruction.DAD(RegSP)          // HL = HL + SP
            | 0x3Auy -> Instruction.LDA(iw)             // A = (word)
            | 0x3Buy -> Instruction.DCX(RegSP)          // SP = SP - 1
            | 0x3Cuy -> Instruction.INR(RegA)           // A = A + 1
            | 0x3Duy -> Instruction.DCR(RegA)           // A = A - 1
            | 0x3Euy -> Instruction.MVI(RegA, ib)       // A = byte
            | 0x3Fuy -> Instruction.CMC                 // Unset the Carry flag
            | 0x40uy -> Instruction.MOV_R_R(RegB, RegB) // B = B
            | 0x41uy -> Instruction.MOV_R_R(RegB, RegC) // B = C
            | 0x42uy -> Instruction.MOV_R_R(RegB, RegD) // B = D
            | 0x43uy -> Instruction.MOV_R_R(RegB, RegE) // B = E
            | 0x44uy -> Instruction.MOV_R_R(RegB, RegH) // B = H
            | 0x45uy -> Instruction.MOV_R_R(RegB, RegL) // B = L
            | 0x46uy -> Instruction.MOV_R_M(RegB)       // B = (HL)
            | 0x47uy -> Instruction.MOV_R_R(RegB, RegA) // B = A
            | 0x48uy -> Instruction.MOV_R_R(RegC, RegB) // C = B
            | 0x49uy -> Instruction.MOV_R_R(RegC, RegC) // C = C
            | 0x4Auy -> Instruction.MOV_R_R(RegC, RegD) // C = D
            | 0x4Buy -> Instruction.MOV_R_R(RegC, RegE) // C = E
            | 0x4Cuy -> Instruction.MOV_R_R(RegC, RegH) // C = H
            | 0x4Duy -> Instruction.MOV_R_R(RegC, RegL) // C = L
            | 0x4Euy -> Instruction.MOV_R_M(RegC)       // C = (HL)
            | 0x4Fuy -> Instruction.MOV_R_R(RegC, RegA) // C = A
            | 0x50uy -> Instruction.MOV_R_R(RegD, RegB) // D = B
            | 0x51uy -> Instruction.MOV_R_R(RegD, RegC) // D = C
            | 0x52uy -> Instruction.MOV_R_R(RegD, RegD) // D = D
            | 0x53uy -> Instruction.MOV_R_R(RegD, RegE) // D = E
            | 0x54uy -> Instruction.MOV_R_R(RegD, RegH) // D = H
            | 0x55uy -> Instruction.MOV_R_R(RegD, RegL) // D = L
            | 0x56uy -> Instruction.MOV_R_M(RegD)       // D = (HL)
            | 0x57uy -> Instruction.MOV_R_R(RegD, RegA) // D = A
            | 0x58uy -> Instruction.MOV_R_R(RegE, RegB) // E = B
            | 0x59uy -> Instruction.MOV_R_R(RegE, RegC) // E = C
            | 0x5Auy -> Instruction.MOV_R_R(RegE, RegD) // E = D
            | 0x5Buy -> Instruction.MOV_R_R(RegE, RegE) // E = E
            | 0x5Cuy -> Instruction.MOV_R_R(RegE, RegH) // E = H
            | 0x5Duy -> Instruction.MOV_R_R(RegE, RegL) // E = L
            | 0x5Euy -> Instruction.MOV_R_M(RegE)       // E = (HL)
            | 0x5Fuy -> Instruction.MOV_R_R(RegE, RegA) // E = A
            | 0x60uy -> Instruction.MOV_R_R(RegH, RegB) // H = B
            | 0x61uy -> Instruction.MOV_R_R(RegH, RegC) // H = C
            | 0x62uy -> Instruction.MOV_R_R(RegH, RegD) // H = D
            | 0x63uy -> Instruction.MOV_R_R(RegH, RegE) // H = E
            | 0x64uy -> Instruction.MOV_R_R(RegH, RegH) // H = H
            | 0x65uy -> Instruction.MOV_R_R(RegH, RegL) // H = L
            | 0x66uy -> Instruction.MOV_R_M(RegH)       // H = (HL)
            | 0x67uy -> Instruction.MOV_R_R(RegH, RegA) // H = A
            | 0x68uy -> Instruction.MOV_R_R(RegL, RegB) // L = B
            | 0x69uy -> Instruction.MOV_R_R(RegL, RegC) // L = C
            | 0x6Auy -> Instruction.MOV_R_R(RegL, RegD) // L = D
            | 0x6Buy -> Instruction.MOV_R_R(RegL, RegE) // L = E
            | 0x6Cuy -> Instruction.MOV_R_R(RegL, RegH) // L = H
            | 0x6Duy -> Instruction.MOV_R_R(RegL, RegL) // L = L
            | 0x6Euy -> Instruction.MOV_R_M(RegL)       // L = (HL)
            | 0x6Fuy -> Instruction.MOV_R_R(RegL, RegA) // L = A
            | 0x70uy -> Instruction.MOV_M_R(RegB)       // (HL) = B
            | 0x71uy -> Instruction.MOV_M_R(RegC)       // (HL) = C
            | 0x72uy -> Instruction.MOV_M_R(RegD)       // (HL) = D
            | 0x73uy -> Instruction.MOV_M_R(RegE)       // (HL) = E
            | 0x74uy -> Instruction.MOV_M_R(RegH)       // (HL) = H
            | 0x75uy -> Instruction.MOV_M_R(RegL)       // (HL) = L
            | 0x76uy -> Instruction.HLT                 // Halt CPU
            | 0x77uy -> Instruction.MOV_M_R(RegA)       // (HL) = A
            | 0x78uy -> Instruction.MOV_R_R(RegA, RegB) // A = B
            | 0x79uy -> Instruction.MOV_R_R(RegA, RegC) // A = C
            | 0x7Auy -> Instruction.MOV_R_R(RegA, RegD) // A = D
            | 0x7Buy -> Instruction.MOV_R_R(RegA, RegE) // A = E
            | 0x7Cuy -> Instruction.MOV_R_R(RegA, RegH) // A = H
            | 0x7Duy -> Instruction.MOV_R_R(RegA, RegL) // A = L
            | 0x7Euy -> Instruction.MOV_R_M(RegA)       // A = (HL)
            | 0x7Fuy -> Instruction.MOV_R_R(RegA, RegA) // A = A
            | 0x80uy -> Instruction.ADD(RegB)           // A = A + B
            | 0x81uy -> Instruction.ADD(RegC)           // A = A + C
            | 0x82uy -> Instruction.ADD(RegD)           // A = A + D
            | 0x83uy -> Instruction.ADD(RegE)           // A = A + E
            | 0x84uy -> Instruction.ADD(RegH)           // A = A + H
            | 0x85uy -> Instruction.ADD(RegL)           // A = A + L
            | 0x86uy -> Instruction.ADD_M               // A = A + (word)
            | 0x87uy -> Instruction.ADD(RegA)           // A = A + A
            | 0x88uy -> Instruction.ADC(RegB)           // A = A + B with Carry
            | 0x89uy -> Instruction.ADC(RegC)           // A = A + C with Carry
            | 0x8Auy -> Instruction.ADC(RegD)           // A = A + D with Carry
            | 0x8Buy -> Instruction.ADC(RegE)           // A = A + E with Carry
            | 0x8Cuy -> Instruction.ADC(RegH)           // A = A + H with Carry
            | 0x8Duy -> Instruction.ADC(RegL)           // A = A + L with Carry
            | 0x8Euy -> Instruction.ADC_M               // A = A + (HL) with Carry
            | 0x8Fuy -> Instruction.ADC(RegA)           // A = A + A with Carry
            | 0x90uy -> Instruction.SUB(RegB)           // A = A - B
            | 0x91uy -> Instruction.SUB(RegC)           // A = A - C
            | 0x92uy -> Instruction.SUB(RegD)           // A = A - D
            | 0x93uy -> Instruction.SUB(RegE)           // A = A - E
            | 0x94uy -> Instruction.SUB(RegH)           // A = A - H
            | 0x95uy -> Instruction.SUB(RegL)           // A = A - L
            | 0x96uy -> Instruction.SUB_M               // A = A - (HL)
            | 0x97uy -> Instruction.SUB(RegA)           // A = A - A
            | 0x98uy -> Instruction.SBB(RegB)           // A = A - B with Borrow
            | 0x99uy -> Instruction.SBB(RegC)           // A = A - C with Borrow
            | 0x9Auy -> Instruction.SBB(RegD)           // A = A - D with Borrow
            | 0x9Buy -> Instruction.SBB(RegE)           // A = A - E with Borrow
            | 0x9Cuy -> Instruction.SBB(RegH)           // A = A - H with Borrow
            | 0x9Duy -> Instruction.SBB(RegL)           // A = A - L with Borrow
            | 0x9Euy -> Instruction.SBB_M               // A = A - (HL) with Borrow
            | 0x9Fuy -> Instruction.SBB(RegA)           // A = A - A with Borrow
            | 0xA0uy -> Instruction.ANA(RegB)           // A = A AND B
            | 0xA1uy -> Instruction.ANA(RegC)           // A = A AND C
            | 0xA2uy -> Instruction.ANA(RegD)           // A = A AND D
            | 0xA3uy -> Instruction.ANA(RegE)           // A = A AND E
            | 0xA4uy -> Instruction.ANA(RegH)           // A = A AND H
            | 0xA5uy -> Instruction.ANA(RegL)           // A = A AND L
            | 0xA6uy -> Instruction.ANA_M               // A = A AND (HL)
            | 0xA7uy -> Instruction.ANA(RegA)           // A = A AND A
            | 0xA8uy -> Instruction.XRA(RegB)           // A = A XOR B
            | 0xA9uy -> Instruction.XRA(RegC)           // A = A XOR C
            | 0xAAuy -> Instruction.XRA(RegD)           // A = A XOR D
            | 0xABuy -> Instruction.XRA(RegE)           // A = A XOR E
            | 0xACuy -> Instruction.XRA(RegH)           // A = A XOR H
            | 0xADuy -> Instruction.XRA(RegL)           // A = A XOR L
            | 0xAEuy -> Instruction.XRA_M               // A = A XOR (HL)
            | 0xAFuy -> Instruction.XRA(RegA)           // A = A XOR A
            | 0xB0uy -> Instruction.ORA(RegB)           // A = A OR B
            | 0xB1uy -> Instruction.ORA(RegC)           // A = A OR C
            | 0xB2uy -> Instruction.ORA(RegD)           // A = A OR D
            | 0xB3uy -> Instruction.ORA(RegE)           // A = A OR E
            | 0xB4uy -> Instruction.ORA(RegH)           // A = A OR H
            | 0xB5uy -> Instruction.ORA(RegL)           // A = A OR L
            | 0xB6uy -> Instruction.ORA_M               // A = A OR (HL)
            | 0xB7uy -> Instruction.ORA(RegA)           // A = A OR A
            | 0xB8uy -> Instruction.CMP(RegB)           // Compare B to A
            | 0xB9uy -> Instruction.CMP(RegC)           // Compare C to A
            | 0xBAuy -> Instruction.CMP(RegD)           // Compare D to A
            | 0xBBuy -> Instruction.CMP(RegE)           // Compare E to A
            | 0xBCuy -> Instruction.CMP(RegH)           // Compare H to A
            | 0xBDuy -> Instruction.CMP(RegL)           // Compare L to A
            | 0xBEuy -> Instruction.CMP_M               // Compare (HL) to A
            | 0xBFuy -> Instruction.CMP(RegA)           // Compare A to A
            | 0xC0uy -> Instruction.RNZ                 // RET if Z flag is not set
            | 0xC1uy -> Instruction.POP(RegBC)          // BC = POP stack
            | 0xC2uy -> Instruction.JNZ(iw)             // Jump to address if Z flag is not set
            | 0xC3uy -> Instruction.JMP(iw)             // Jump to address
            | 0xC4uy -> Instruction.CNZ(iw)             // CALL address if Z flag is not set
            | 0xC5uy -> Instruction.PUSH(RegBC)         // PUSH BC to stack
            | 0xC6uy -> Instruction.ADI(ib)             // A = A + byte
            | 0xC7uy -> Instruction.RST(0uy)            // RST 0
            | 0xC8uy -> Instruction.RZ                  // RET if Z flag is set
            | 0xC9uy -> Instruction.RET                 // Return from call
            | 0xCAuy -> Instruction.JZ(iw)              // Jump to address if Z flag is set
            | 0xCBuy -> Instruction.JMP(iw)             // Alernate for JMP (do not use)
            | 0xCCuy -> Instruction.CZ(iw)              // CALL address if Z flag is set
            | 0xCDuy -> Instruction.CALL(iw)            // Push PC to stack and jump to address
            | 0xCEuy -> Instruction.ACI(ib)             // A = A + byte with Carry
            | 0xCFuy -> Instruction.RST(1uy)            // RST 1
            | 0xD0uy -> Instruction.RNC                 // RET if C flag not set
            | 0xD1uy -> Instruction.POP(RegDE)          // DE = POP stack
            | 0xD2uy -> Instruction.JNC(iw)             // Jump to address if C flag is not set
            | 0xD3uy -> Instruction.OUT(ib)             // Send A to device
            | 0xD4uy -> Instruction.CNC(iw)             // CALL memory address if C flag is not set
            | 0xD5uy -> Instruction.PUSH(RegDE)         // PUSH DE to stack
            | 0xD6uy -> Instruction.SUI(ib)             // A = A - byte
            | 0xD7uy -> Instruction.RST(2uy)            // RST 2
            | 0xD8uy -> Instruction.RC                  // RET if C flag is set
            | 0xD9uy -> Instruction.RET                 // Alternative for RET (do not use)
            | 0xDAuy -> Instruction.JC(iw)              // JUMP to address if C flag is set
            | 0xDBuy -> Instruction.IN(ib)              // A = device output
            | 0xDCuy -> Instruction.CC(iw)              // CALL address if C flag is set
            | 0xDDuy -> Instruction.CALL(iw)            // Alternative for CALL (do not use)
            | 0xDEuy -> Instruction.SBI(ib)             // A = A - byte with Borrow
            | 0xDFuy -> Instruction.RST(3uy)            // RST 3
            | 0xE0uy -> Instruction.RPO                 // RET if Odd (Parity flag not set)
            | 0xE1uy -> Instruction.POP(RegHL)          // HL = POP stack
            | 0xE2uy -> Instruction.JPO(iw)             // JUMP to address if Odd (Parity flag not set)
            | 0xE3uy -> Instruction.XTHL                // Exchange Stack with HL
            | 0xE4uy -> Instruction.CPO(iw)             // CALL address if Odd (Parity flag not set)
            | 0xE5uy -> Instruction.PUSH(RegHL)         // PUSH HL to stack
            | 0xE6uy -> Instruction.ANI(ib)             // A = A AND byte
            | 0xE7uy -> Instruction.RST(4uy)            // RST 4
            | 0xE8uy -> Instruction.RPE                 // RET if Even (Parity flag set)
            | 0xE9uy -> Instruction.PCHL                // PC = HL
            | 0xEAuy -> Instruction.JPE(iw)             // JUMP to address if Even (Parity flag set)
            | 0xEBuy -> Instruction.XCHG                // Exchange HL and DE
            | 0xECuy -> Instruction.CPE(iw)             // CALL address if Even (Parity flag set)
            | 0xEDuy -> Instruction.CALL(iw)            // Alternative for CALL (do not use)
            | 0xEEuy -> Instruction.XRI(ib)             // A = A XOR byte
            | 0xEFuy -> Instruction.RST(5uy)            // RST 5
            | 0xF0uy -> Instruction.RP                  // RET if positive (S flag not set)
            | 0xF1uy -> Instruction.POP_PSW             // POP A and FLAGS from stack
            | 0xF2uy -> Instruction.JP(iw)              // JUMP to address if Positive (S flag not set)
            | 0xF3uy -> Instruction.DI                  // Disable interrupts
            | 0xF4uy -> Instruction.CP(iw)              // ALL if Positive (Sign flag not set)
            | 0xF5uy -> Instruction.PUSH_PSW            // PUSH A and FLAGS to stack
            | 0xF6uy -> Instruction.ORI(ib)             // A = X OR byte
            | 0xF7uy -> Instruction.RST(6uy)            // RST 6
            | 0xF8uy -> Instruction.RM                  // RET if minus (S flag set)
            | 0xF9uy -> Instruction.SPHL                // SP = HL
            | 0xFAuy -> Instruction.JM(iw)              // JUMP to address if minus (S flag set)
            | 0xFBuy -> Instruction.EI                  // Enable instructions
            | 0xFCuy -> Instruction.CM(iw)              // CALL if minus (S flag set)
            | 0xFDuy -> Instruction.CALL(iw)            // Alternative for CALL (do not use)
            | 0xFEuy -> Instruction.CPI(ib)             // Compare byte to A
            | 0xFFuy -> Instruction.RST(7uy)            // RST 7
            | _ -> raise (UnknownInstruction(opcode))

    // Carries out the given instruction
    // Returns the post-execution state, memory, and number of cycles it took to complete the instruction
    member private this.Execute instruction (cpu, memory) =
        let ex f =
            f cpu
            |> fun (cpu, cycles) -> (cpu, memory, cycles)

        let exm f =
            f cpu memory

        let exrm f =
            f cpu memory
            |> fun (cpu, cycles) -> (cpu, memory, cycles)

        match cpu.State with
        | Stopped -> None
        | Halted -> Some (cpu, memory, 1)
        | Running ->
            executionEvent.Trigger(instruction, cpu, memory)

            Some <| match instruction with
                    | NOP -> ex nop
                    | LXI(reg, value) -> ex (lxi reg value)
                    | STAX(reg) -> exm (stax reg)
                    | INX(reg) -> ex (inx reg)
                    | INR(reg) -> ex (inr reg)
                    | DCR(reg) -> ex (dcr reg)
                    | MVI(reg, value) -> ex (mvi reg value)
                    | RLC -> ex rlc
                    | DAD(reg) -> ex (dad reg) 
                    | LDAX(reg) -> exrm (ldax reg)
                    | DCX(reg) -> ex (dcx reg)
                    | RRC -> ex rrc
                    | RAL -> ex ral
                    | RAR -> ex rar
                    | SHLD(address) -> exm (shld address) 
                    | DAA -> ex daa
                    | LHLD(address) -> exrm (lhld address) 
                    | CMA -> ex cma
                    | STA(address) -> exm (sta address) 
                    | INR_M -> exm inr_m 
                    | DCR_M -> exm dcr_m 
                    | MVI_M(value) -> exm (mvi_m value) 
                    | STC -> ex stc
                    | LDA(address) -> exrm (lda address) 
                    | CMC -> ex cmc
                    | MOV_R_R(dest, src) -> ex (mov_r_r dest src)
                    | MOV_R_M(reg) -> exrm (mov_r_m reg)
                    | MOV_M_R(reg) -> exm (mov_m_r reg)
                    | HLT -> ex hlt 
                    | ADD(reg) -> ex (add reg)
                    | ADD_M -> exrm add_m
                    | ADC(reg) -> ex (adc reg)
                    | ADC_M -> exrm adc_m
                    | SUB(reg) -> ex (sub reg)
                    | SUB_M -> exrm sub_m
                    | SBB(reg) -> ex (sbb reg)
                    | SBB_M -> exrm sbb_m
                    | ANA(reg) -> ex (ana reg)
                    | ANA_M -> exrm ana_m
                    | XRA(reg) -> ex (xra reg)
                    | XRA_M  -> exrm xra_m
                    | ORA(reg) -> ex (ora reg)
                    | ORA_M -> exrm ora_m
                    | CMP(reg) -> ex (cmp reg)
                    | CMP_M -> exrm cmp_m
                    | RNZ -> exrm rnz
                    | POP(reg) -> exrm (pop reg)
                    | JNZ(address) -> ex (jnz address)
                    | JMP(address) -> ex (jmp address)
                    | CNZ(address) -> exm (cnz address)
                    | PUSH(reg) -> exm (push reg)
                    | ADI(byte) -> ex (adi byte)
                    | RST(byte) -> exm (rst byte)
                    | RZ -> exrm rz
                    | RET -> exrm ret
                    | JZ(address) -> ex (jz address)
                    | CZ(address) -> exm (cz address)
                    | CALL(address) -> exm (call address)
                    | ACI(byte) -> ex (aci byte)
                    | RNC -> exrm rnc
                    | JNC(address) -> ex (jnc address)
                    | OUT(device) -> 
                        (ex out) 
                        |> fun (cpu, memory, cycles) -> 
                            (this.SetOutput device cpu.A)
                            (cpu, memory, cycles)
                    | CNC(address) -> exm (cnc address)
                    | SUI(byte) -> ex (sui byte)
                    | RC -> exrm rc
                    | JC(address) -> ex (jc address)
                    | IN(device) -> ex (in' (this.GetInput device))
                    | CC(address) -> exm (cc address)
                    | SBI(byte) -> ex (sbi byte)
                    | RPO -> exrm rpo
                    | JPO(address) -> ex (jpo address)
                    | XTHL -> exm xthl
                    | CPO(address) -> exm (cpo address)
                    | ANI(byte) -> ex (ani byte)
                    | RPE -> exrm rpe
                    | PCHL -> ex pchl
                    | JPE(address) -> ex (jpe address)
                    | XCHG -> ex xchg
                    | CPE(address) -> exm (cpe address)
                    | XRI(byte) -> ex (xri byte)
                    | RP -> exrm rp
                    | POP_PSW -> exrm pop_psw
                    | JP(address) -> ex (jp address)
                    | DI -> ex di
                    | CP(address) -> exm (cp address)
                    | PUSH_PSW -> exm push_psw
                    | ORI(byte) -> ex (ori byte)
                    | RM -> exrm rm
                    | SPHL -> ex sphl
                    | JM(address) -> ex (jm address)
                    | EI -> ex ei
                    | CM(address) -> exm (cm address)
                    | CPI(byte) -> ex (cpi byte)

    // Get byte from input device
    member private this.GetInput port =
        if inputDevices.ContainsKey port
        then (inputDevices.Item port)()
        else raise (UnknownDevice(port))

    // Push byte to output device
    member private this.SetOutput port value =
        if outputDevices.ContainsKey port
        then (outputDevices.Item port) value
        else raise (UnknownDevice(port))

    // Get the next interrupt to be executed
    member private this.TryInterrupt =
        match interruptRequests with
                | head::tail -> 
                    interruptRequests <- tail
                    Some head
                | [] -> None

    // Wastes CPU time in attempt to match the emulated CPU's speed
    member private this.Wait cycles =
        let emuNano = (float)watch.ElapsedTicks / frequencyNano
        let cpuNano = (float)cycles / (this.Hz / 1000000.0)

        if emuNano < cpuNano then 
            let ticks = (int64)((cpuNano - emuNano) * frequencyNano)
            while watch.ElapsedTicks < ticks do 0 |> ignore

    // Attaches an input device to the specified port
    member this.AttachInput (port: byte, device) =
        inputDevices <- inputDevices.Add(port, device)

    // Attaches an output device to the specified port
    member this.AttachOutput (port, device) =
        outputDevices <- outputDevices.Add(port, device)
           
    // Run the emulator
    member this.Run (memory:Memory, pc: uint16) =
        let cpu = { defaultCpu with PC = { High = 0uy; Low = 0uy; } + pc; State = Running };

        stopping <- false

        let rec cycle instruction (cpu, memory) =
            currentCpu <- 
                if stopping 
                then { cpu with State = Stopped; } 
                else cpu

            cycleEvent.Trigger()

            watch.Reset()
            watch.Start()

            let postExecute = this.Execute instruction (currentCpu, memory)

            match postExecute with
            | None -> (cpu, memory)
            | Some (cpu, memory, cycles) ->
                this.Wait cycles

                postExecutionEvent.Trigger(instruction, cpu, memory)

                // Carry out an interrupt if one is available, else continue with the next instruction in memory
                match this.TryInterrupt with
                    | Some instruction -> 
                        // If CPU is in Halted state, an interrupt knocks it back into Running. 
                        // Also, disable interrupts when an interrupt occurs
                        { cpu with INTE = false; }
                        |> fun cpu -> if cpu.State = Halted then { cpu with State = Running } else cpu
                        |> fun cpu -> cycle instruction (cpu, memory)
                    | None -> 
                        this.FetchAndDecode (cpu, memory)
                        |> fun instruction -> cycle instruction (cpu, memory)
                
        // Start the cycle
        this.FetchAndDecode (cpu, memory)
        |> fun instruction -> cycle instruction (cpu, memory)
        |> ignore
            
        watch.Stop()

    // Causes an interrupt
    member this.Interrupt instruction =
        if currentCpu.INTE then
            interruptRequests <- interruptRequests @ [instruction]

    // Stops the emulator
    member this.Stop =
        stopping <- true