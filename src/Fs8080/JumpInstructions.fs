module Fs8080.JumpInstructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

// RET if Z flag is not set
let rnz cpu memory =
    if cpu.FLAGS &&& FlagMask.Z = 0uy then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5

// Jump to address if Z flag is not set
let jnz address cpu =
    let nextpc = 
        if (cpu.FLAGS &&& FlagMask.Z) = 0uy
        then address
        else cpu.PC + 3us

    set16 PC nextpc cpu, 10

// Jump to address
let jmp address cpu =
    { cpu with PC = address }
    |> fun cpu -> cpu, 10        

// RET if Z flag is set
let rz cpu memory =
    if cpu.FLAGS &&& FlagMask.Z = FlagMask.Z then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 15
    else
        incPC 1us cpu, 11

// POP PC off stack and jump to it
let ret cpu memory =
    let pc = {
        High = (fetch (cpu.SP+1us).Value memory);
        Low = (fetch cpu.SP.Value memory);
    }

    set16 PC pc cpu
    |> set16 SP (cpu.SP + 2us), 1

// Jump to address if Z flag is set
let jz address cpu =
    let nextpc = 
        if (cpu.FLAGS &&& FlagMask.Z) = FlagMask.Z
        then address
        else cpu.PC + 3us

    set16 PC nextpc cpu, 1

// Push PC to stack and jump to address
let call address cpu memory =
    let nextpc = cpu.PC + 3us

    let memory =
        store (cpu.SP - 1us).Value nextpc.High memory
        |> store (cpu.SP - 2us).Value nextpc.Low

    { cpu with PC = address; SP = cpu.SP - 2us }
    |> fun cpu -> cpu, memory, 17

// CALL if Z is not set
let cnz address cpu memory =
    if (cpu.FLAGS &&& FlagMask.Z) = 0uy
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RST
let rst (num: byte) cpu memory =
    let memory =
        store (cpu.SP - 1us).Value cpu.PC.High memory
        |> store (cpu.SP - 2us).Value cpu.PC.Low

    let address = { Low = (num * 8uy); High = 0uy }

    { cpu with PC = address; SP = cpu.SP - 2us }
    |> fun cpu -> cpu, memory, 11

// CALL if Z is set
let cz address cpu memory =
    if (cpu.FLAGS &&& FlagMask.Z) = FlagMask.Z
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RET if C is not set
let rnc cpu memory = 
    if cpu.FLAGS &&& FlagMask.C = 0uy then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5

// Jump to address if C flag is not set
let jnc address cpu =
    let pc = 
        if cpu.FLAGS &&& FlagMask.C = 0uy then
           address
        else
            cpu.PC + 3us

    set16 PC pc cpu, 10

// CALL if C flag is not set
let cnc address cpu memory =
    if (cpu.FLAGS &&& FlagMask.C) = 0uy
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RET if Carry flag set
let rc cpu memory =
    if cpu.FLAGS &&& FlagMask.C = FlagMask.C then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5

// Jump to address if C flag is set
let jc address cpu =
    let pc = 
        if cpu.FLAGS &&& FlagMask.C = FlagMask.C then
           address
        else
            cpu.PC + 3us

    set16 PC pc cpu, 10

// CALL if C flag is set
let cc address cpu memory =
    if (cpu.FLAGS &&& FlagMask.C) = FlagMask.C
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RET if  Odd (Partity flag not set)
let rpo cpu memory =
    if cpu.FLAGS &&& FlagMask.P = 0uy then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5

// Jump to address if Odd (Parity flag not set)
let jpo address cpu =
    let pc = 
        if cpu.FLAGS &&& FlagMask.P = 0uy then
           address
        else
            cpu.PC + 3us

    set16 PC pc cpu, 10

// CALL if P flag is not set
let cpo address cpu memory =
    if (cpu.FLAGS &&& FlagMask.P) = 0uy
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RET if Even (Partity flag set)
let rpe cpu memory =
    if cpu.FLAGS &&& FlagMask.P = FlagMask.P then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us)
        |> fun cpu -> cpu, 11
    else
        incPC 1us cpu, 5

// JUMP to address if Even (Parity flag set)
let jpe address cpu =
    let pc = 
        if cpu.FLAGS &&& FlagMask.P = FlagMask.P then
           address
        else
            cpu.PC + 3us

    set16 PC pc cpu, 10

// CALL if Even (Parity flag set)
let cpe address cpu memory =
    if (cpu.FLAGS &&& FlagMask.P) = FlagMask.P
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RET if positive (S flag not set)
let rp cpu memory =
    if cpu.FLAGS &&& FlagMask.S = 0uy then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5

// JUMP to address if Positive (Sign flag not set)
let jp address cpu =
    let pc = 
        if cpu.FLAGS &&& FlagMask.S = 0uy then
           address
        else
            cpu.PC + 3us

    set16 PC pc cpu, 10

// CALL if Positive (Sign flag not set)
let cp address cpu memory =
    if (cpu.FLAGS &&& FlagMask.S) = 0uy
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11

// RET if minus (S flag set)
let rm cpu memory =
    if cpu.FLAGS &&& FlagMask.S = FlagMask.S then
        let pc = {
            High = (fetch (cpu.SP+1us).Value memory);
            Low = (fetch cpu.SP.Value memory);
        }

        set16 PC pc cpu
        |> set16 SP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5

// JUMP to address if Minus (Sign flag set)
let jm address cpu =
    let pc = 
        if cpu.FLAGS &&& FlagMask.S = FlagMask.S then
           address
        else
            cpu.PC + 3us

    set16 PC pc cpu, 10

// CALL if Minus (Sign flag set)
let cm address cpu memory =
    if (cpu.FLAGS &&& FlagMask.S) = FlagMask.S
    then 
        call address cpu memory
    else
        incPC 3us cpu
        |> fun cpu -> cpu, memory, 11