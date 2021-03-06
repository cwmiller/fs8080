﻿module Fs8080.Types

open System

// Holds 16 bit values as two separate bytes with the high bits always in High and the low bits in Low.
type DWord = 
    {
        High: byte;
        Low: byte
    }

    // Returns the value of this DWord with respect to the host system's endianness
    member this.Value =
        if BitConverter.IsLittleEndian
            then BitConverter.ToUInt16([|this.Low; this.High|], 0)
            else BitConverter.ToUInt16([|this.High; this.Low|], 0)

    // Add a value (in host's endianness) to DWord
    static member (+) (dword: DWord, amt: uint16) =
        let bytes = BitConverter.GetBytes(dword.Value + amt)
        DWord.fromBytes bytes

    // Subtract a value (in host's endianness) to DWord
    static member (-) (dword: DWord, amt: uint16) =
        let bytes = BitConverter.GetBytes(dword.Value - amt)
        DWord.fromBytes bytes

    // Add two DWords together
    static member (+) (dword: DWord, amt: DWord) =
        let bytes = BitConverter.GetBytes(dword.Value + amt.Value)
        DWord.fromBytes bytes

    // Subtract one DWord from another
    static member (-) (dword: DWord, amt: DWord) =
        let bytes = BitConverter.GetBytes(dword.Value - amt.Value)
        DWord.fromBytes bytes

    // Takes a byte array in the host's endianness and returns a DWord record in big-endian
    static member fromBytes bytes =
        if BitConverter.IsLittleEndian
        then { High = (Array.get bytes 1); Low = (Array.get bytes 0); }
        else { High = (Array.get bytes 0); Low = (Array.get bytes 1); }


// List of 8bit registers
and Register8 =
    | RegA
    | RegB
    | RegC
    | RegD
    | RegE
    | RegH
    | RegL
    | RegFLAGS


// List of 16bit registers
and Register16 =
    | RegAF
    | RegBC
    | RegDE
    | RegHL
    | RegSP
    | RegPC


// Bitmasks used for setting/getting bits on FLAGS register
and FlagMask = 
    static member S = 0x80uy
    static member Z = 0x40uy
    static member A = 0x10uy
    static member P = 0x04uy
    static member C = 0x01uy


and State =
    | Stopped
    | Running
    | Halted


// Represents the CPU's current state
and Cpu = 
    {
        // Registers
        A: byte;
        B: byte;
        C: byte;
        D: byte;
        E: byte;
        H: byte;
        L: byte;
        SP: DWord;
        PC: DWord;
        FLAGS: byte;

        // Interrupts enabled or disabled
        INTE: bool;

        // Running state
        State: State 
    }

    // Getters for easy reading of 16bit registers and flags
    member this.AF = { High = this.A; Low = this.FLAGS }
    member this.BC = { High = this.B; Low = this.C }
    member this.DE = { High = this.D; Low = this.E }
    member this.HL = { High = this.H; Low = this.L }

    member this.FlagS = (this.FLAGS &&& FlagMask.S) = FlagMask.S
    member this.FlagZ = (this.FLAGS &&& FlagMask.Z) = FlagMask.Z
    member this.FlagA = (this.FLAGS &&& FlagMask.A) = FlagMask.A
    member this.FlagP = (this.FLAGS &&& FlagMask.P) = FlagMask.P
    member this.FlagC = (this.FLAGS &&& FlagMask.C) = FlagMask.C


and Flag = 
    | FlagS
    | FlagZ
    | FlagA
    | FlagP
    | FlagC


// List of supported instructions
and Instruction =
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
    | RST of byte
    | RZ
    | RET
    | JZ of DWord
    | CZ of DWord
    | CALL of DWord
    | ACI of byte 
    | RNC
    | JNC of DWord
    | OUT of byte
    | CNC of DWord
    | SUI of byte
    | RC
    | JC of DWord
    | IN of byte
    | CC of DWord
    | SBI of byte
    | RPO
    | JPO of DWord
    | XTHL
    | CPO of DWord
    | ANI of byte
    | RPE
    | PCHL
    | JPE of DWord
    | XCHG
    | CPE of DWord
    | XRI of byte
    | RP
    | POP_PSW
    | JP of DWord
    | DI
    | CP of DWord
    | PUSH_PSW
    | ORI of byte
    | RM
    | SPHL
    | JM of DWord
    | EI
    | CM of DWord
    | CPI of byte


and Memory = Map<uint16,byte>

and System = Cpu * Memory

and InputDevice = (unit -> byte)

and OutputDevice = (byte -> unit)