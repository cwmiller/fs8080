module Fs8080.Registers

open Fs8080.Types

// Gets the value of an 8bit register
let get8 register cpu = 
    match register with
        | A -> cpu.A
        | B -> cpu.B
        | C -> cpu.C
        | D -> cpu.D
        | E -> cpu.E
        | H -> cpu.H
        | L -> cpu.L
        | FLAGS -> cpu.FLAGS

// Gets the value of a 16bit register
let get16 register cpu = 
    match register with
        | AF -> { High = cpu.A; Low = cpu.FLAGS }
        | BC -> { High = cpu.B; Low = cpu.C }
        | DE -> { High = cpu.D; Low = cpu.E }
        | HL -> { High = cpu.H; Low = cpu.L }
        | SP -> cpu.SP
        | PC -> cpu.PC

// Sets the value of an 8bit register
let set8 register value cpu =
    match register with
        | A -> { cpu with A = value }
        | B -> { cpu with B = value }
        | C -> { cpu with C = value }
        | D -> { cpu with D = value }
        | E -> { cpu with E = value }
        | H -> { cpu with H = value }
        | L -> { cpu with L = value }
        | FLAGS -> { cpu with FLAGS = value }

// Sets the value of a 16bit register
let set16 register (value: DWord) cpu =
    match register with
        | AF -> { cpu with A = value.High; FLAGS = value.Low; }
        | BC -> { cpu with B = value.High; C = value.Low; }
        | DE -> { cpu with D = value.High; E = value.Low; }
        | HL -> { cpu with H = value.High; L = value.Low; }
        | SP -> { cpu with SP = value; }
        | PC -> { cpu with PC = value; }

// Copy the value from one 8bit register to another
let copy8 src dest cpu =
    get8 src cpu
    |> fun value -> set8 dest value cpu

// Increment PC register
let incPC (amt: uint16) cpu = 
    { cpu with PC = cpu.PC + amt }

// Set S flag based on value
let flagS (value: byte) cpu =
    if (value &&& 0x80uy) > 0uy
    then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.S }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.S }

// Set Z flag based on value
let flagZ (value: byte) cpu = 
    if value = 0uy 
    then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.Z; }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.Z; }

// Set P flag based on value
let flagP (value: byte) cpu =
    let cnt = 
        [0..7]
        |> List.fold (fun acc idx -> acc + (value >>> idx) &&& 0x1uy) 0uy

    if cnt % 2uy = 0uy
    then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.P; }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.P; }

// Set A flag based on value
let flagA (value: byte) cpu =
    if (value &&& 0x0Fuy) = 0uy
    then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.A; }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.A; }

// Set C flag based on addition result (flag is set is addition overflows)
let flagCForAdd (previousValue: byte) (value: byte) cpu =
    if (value < previousValue)
    then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }

// Set C flag based on subtraction result (flag is set is addition overflows)
let flagCForSub (previousValue: byte) (value: byte) cpu =
    if (value > previousValue)
    then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }
    else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }

// Set S, Z, A, and P flags based on value
let flagSZAP (value: byte) cpu =
    flagS value cpu
    |> flagZ value
    |> flagP value
    |> flagA value