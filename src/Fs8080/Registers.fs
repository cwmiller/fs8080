module Fs8080.Registers

open Fs8080.Types

// Gets the value of an 8bit register
let get8 register cpu = 
    match register with
    | RegA -> cpu.A
    | RegB -> cpu.B
    | RegC -> cpu.C
    | RegD -> cpu.D
    | RegE -> cpu.E
    | RegH -> cpu.H
    | RegL -> cpu.L
    | RegFLAGS -> cpu.FLAGS


// Gets the value of a 16bit register
let get16 register cpu = 
    match register with
    | RegAF -> { High = cpu.A; Low = cpu.FLAGS }
    | RegBC -> { High = cpu.B; Low = cpu.C }
    | RegDE -> { High = cpu.D; Low = cpu.E }
    | RegHL -> { High = cpu.H; Low = cpu.L }
    | RegSP -> cpu.SP
    | RegPC -> cpu.PC


// Sets the value of an 8bit register
let set8 register value cpu =
    match register with
    | RegA -> { cpu with A = value }
    | RegB -> { cpu with B = value }
    | RegC -> { cpu with C = value }
    | RegD -> { cpu with D = value }
    | RegE -> { cpu with E = value }
    | RegH -> { cpu with H = value }
    | RegL -> { cpu with L = value }
    | RegFLAGS -> { cpu with FLAGS = value }


// Sets the value of a 16bit register
let set16 register (value: DWord) cpu =
    match register with
    | RegAF -> { cpu with A = value.High; FLAGS = value.Low; }
    | RegBC -> { cpu with B = value.High; C = value.Low; }
    | RegDE -> { cpu with D = value.High; E = value.Low; }
    | RegHL -> { cpu with H = value.High; L = value.Low; }
    | RegSP -> { cpu with SP = value; }
    | RegPC -> { cpu with PC = value; }


// Copy the value from one 8bit register to another
let copy8 src dest cpu =
    get8 src cpu
    |> fun value -> set8 dest value cpu


// Increment PC register
let incPC (amt: uint16) cpu = 
    { cpu with PC = cpu.PC + amt }


let flagMask = function
    | FlagS -> FlagMask.S
    | FlagZ -> FlagMask.Z
    | FlagA -> FlagMask.A
    | FlagP -> FlagMask.P
    | FlagC -> FlagMask.C


// Flag/unflag a bit in FLAGS
let setFlag flag enabled cpu =
    let mask = flagMask flag

    if enabled = true
    then { cpu with FLAGS = cpu.FLAGS ||| mask }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~mask }


// Get the status of a bit in FLAGS
let getFlag flag cpu =
    let mask = flagMask flag

    ((cpu.FLAGS &&& mask) = mask)


// Set S flag based on value
let flagS value =
    setFlag FlagS ((value &&& 0x80uy) > 0uy)


// Set Z flag based on value
let flagZ value = 
    setFlag FlagZ (value = 0uy)


// Set P flag based on value
let flagP value =
    let cnt = 
        [0..7]
        |> List.fold (fun acc idx -> acc + (value >>> idx) &&& 0x1uy) 0uy

    setFlag FlagP (cnt % 2uy = 0uy)


// Set A flag based on value
let flagA value =
    setFlag FlagA ((value &&& 0x0Fuy) = 0uy)


// Set C flag based on addition result (flag is set is addition overflows)
let flagCForAdd (previousValue: byte) (value: byte) =
    setFlag FlagC (value < previousValue)


// Set C flag based on subtraction result (flag is set is addition overflows)
let flagCForSub (previousValue: byte) (value: byte) =
    setFlag FlagC (value > previousValue)


// Set S, Z, A, and P flags based on value
let flagSZAP (value: byte) cpu =
    flagS value cpu
    |> flagZ value
    |> flagP value
    |> flagA value