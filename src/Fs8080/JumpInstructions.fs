module Fs8080.JumpInstructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

// RET if Z flag is not set
let rnz (cpu: Cpu) memory =
    if cpu.FlagZ = false then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5


// Jump to address if Z flag is not set
let jnz address (cpu: Cpu) =
    let nextpc = 
        if cpu.FlagZ = false
        then address
        else cpu.PC + 3us

    set16 RegPC nextpc cpu, 10


// Jump to address
let jmp address cpu =
    { cpu with PC = address }, 10    


// RET if Z flag is set
let rz (cpu: Cpu) memory =
    if cpu.FlagZ 
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> (set16 RegSP (cpu.SP + 2us)), 15
    else
        incPC 1us cpu, 11


// POP PC off stack and jump to it
let ret cpu memory =
    fetch16 cpu.SP.Value memory
    |> fun pc -> set16 RegPC pc cpu
    |> (set16 RegSP (cpu.SP + 2us)), 1


// Jump to address if Z flag is set
let jz address (cpu: Cpu) =
    let nextpc = 
        if cpu.FlagZ
        then address
        else cpu.PC + 3us

    set16 RegPC nextpc cpu, 1


// Push PC to stack and jump to address
let call address cpu memory =
    let memory = store16 (cpu.SP - 2us).Value (cpu.PC + 3us) memory

    { cpu with PC = address; SP = cpu.SP - 2us }
    |> fun cpu -> cpu, memory, 17


// CALL if Z is not set
let cnz address (cpu: Cpu) memory =
    if cpu.FlagZ = false
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RST
let rst (num: byte) cpu memory =
    let memory = store16 (cpu.SP - 2us).Value cpu.PC memory
    let address = { Low = (num * 8uy); High = 0uy }

    { cpu with PC = address; SP = cpu.SP - 2us }
    |> fun cpu -> cpu, memory, 11


// CALL if Z is set
let cz address (cpu: Cpu) memory =
    if cpu.FlagZ
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RET if C is not set
let rnc cpu memory = 
    if (getFlag FlagC cpu) = false 
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5


// Jump to address if C flag is not set
let jnc address (cpu: Cpu) =
    let pc = 
        if cpu.FlagC = false
        then address
        else cpu.PC + 3us

    set16 RegPC pc cpu, 10


// CALL if C flag is not set
let cnc address (cpu: Cpu) memory =
    if cpu.FlagC = false
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RET if Carry flag set
let rc (cpu: Cpu) memory =
    if cpu.FlagC
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5


// Jump to address if C flag is set
let jc address (cpu: Cpu) =
    let pc = 
        if cpu.FlagC
        then address
        else cpu.PC + 3us

    set16 RegPC pc cpu, 10


// CALL if C flag is set
let cc address (cpu: Cpu) memory =
    if cpu.FlagC
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RET if  Odd (Partity flag not set)
let rpo (cpu: Cpu) memory =
    if cpu.FlagP = false
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5


// Jump to address if Odd (Parity flag not set)
let jpo address (cpu: Cpu) =
    let pc = 
        if cpu.FlagP = false
        then address
        else cpu.PC + 3us

    set16 RegPC pc cpu, 10


// CALL if P flag is not set
let cpo address (cpu: Cpu) memory =
    if cpu.FlagP = false
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RET if Even (Partity flag set)
let rpe cpu memory =
    if (getFlag FlagP cpu)
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us)
        |> fun cpu -> cpu, 11
    else
        incPC 1us cpu, 5


// PC = HL
let pchl cpu =
    { cpu with PC = cpu.HL }, 5


// JUMP to address if Even (Parity flag set)
let jpe address (cpu: Cpu) =
    let pc = 
        if cpu.FlagP
        then address
        else cpu.PC + 3us

    set16 RegPC pc cpu, 10


// CALL if Even (Parity flag set)
let cpe address (cpu: Cpu) memory =
    if cpu.FlagP
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RET if positive (S flag not set)
let rp (cpu: Cpu) memory =
    if cpu.FlagS = false
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5


// JUMP to address if Positive (Sign flag not set)
let jp address (cpu: Cpu) =
    let pc = 
        if cpu.FlagS = false
        then address
        else cpu.PC + 3us

    set16 RegPC pc cpu, 10


// CALL if Positive (Sign flag not set)
let cp address (cpu: Cpu) memory =
    if cpu.FlagS = false
    then call address cpu memory
    else incPC 3us cpu, memory, 11


// RET if minus (S flag set)
let rm (cpu: Cpu) memory =
    if cpu.FlagS
    then
        fetch16 cpu.SP.Value memory
        |> fun pc -> set16 RegPC pc cpu
        |> set16 RegSP (cpu.SP + 2us), 11
    else
        incPC 1us cpu, 5


// JUMP to address if Minus (Sign flag set)
let jm address (cpu: Cpu) =
    let pc = 
        if cpu.FlagS
        then address
        else cpu.PC + 3us

    set16 RegPC pc cpu, 10


// CALL if Minus (Sign flag set)
let cm address (cpu: Cpu) memory =
    if cpu.FlagS 
    then call address cpu memory
    else incPC 3us cpu, memory, 11