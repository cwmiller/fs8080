module Fs8080.Registers

open Fs8080.Types

// List of 8bit registers
type Register8 =
    | A
    | B
    | C
    | D
    | E
    | H
    | L
    | FLAGS

// List of 16bit registers
type Register16 =
    | AF
    | BC
    | DE
    | HL
    | SP
    | PC

// Bitmasks used for setting/getting bits on FLAGS register
type FlagMask = 
    static member S = 0x80uy
    static member Z = 0x40uy
    static member A = 0x10uy
    static member P = 0x04uy
    static member C = 0x01uy

// Gets the value of an 8bit register
let get8 register state = 
    match register with
        | A -> state.A
        | B -> state.B
        | C -> state.C
        | D -> state.D
        | E -> state.E
        | H -> state.H
        | L -> state.L
        | FLAGS -> state.FLAGS

// Gets the value of a 16bit register
let get16 register state = 
    match register with
        | AF -> { High = state.A; Low = state.FLAGS }
        | BC -> { High = state.B; Low = state.C }
        | DE -> { High = state.D; Low = state.E }
        | HL -> { High = state.H; Low = state.L }
        | SP -> state.SP
        | PC -> state.PC

// Sets the value of an 8bit register
let set8 register value state =
    match register with
        | A -> { state with A = value }
        | B -> { state with B = value }
        | C -> { state with C = value }
        | D -> { state with D = value }
        | E -> { state with E = value }
        | H -> { state with H = value }
        | L -> { state with L = value }
        | FLAGS -> { state with FLAGS = value }

// Sets the value of a 16bit register
let set16 register (value: DWord) state =
    match register with
        | AF -> { state with A = value.High; FLAGS = value.Low; }
        | BC -> { state with B = value.High; C = value.Low; }
        | DE -> { state with D = value.High; E = value.Low; }
        | HL -> { state with H = value.High; L = value.Low; }
        | SP -> { state with SP = value; }
        | PC -> { state with PC = value; }

// Copy the value from one 8bit register to another
let copy8 src dest state =
    get8 src state
    |> fun value -> set8 dest value state

// Increment PC register
let incPC (amt: uint16) state = 
    { state with PC = state.PC + amt }

// Increment cycle counter
let incWC amt state = 
    { state with WC = state.WC + amt }

// Set S flag based on value
let flagS (value: byte) state =
    if (value &&& 0x80uy) > 0uy
    then { state with FLAGS = state.FLAGS ||| FlagMask.S }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.S }

// Set Z flag based on value
let flagZ (value: byte) state = 
    if value = 0uy 
    then { state with FLAGS = state.FLAGS ||| FlagMask.Z; }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.Z; }

// Set P flag based on value
let flagP (value: byte) state =
    let cnt = 
        [0..7]
        |> List.fold (fun acc idx -> acc + (value >>> idx) &&& 0x1uy) 0uy

    if cnt % 2uy = 0uy
    then { state with FLAGS = state.FLAGS ||| FlagMask.P; }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.P; }

// Set A flag based on value
let flagA (value: byte) state =
    if (value &&& 0x0Fuy) = 0uy
    then { state with FLAGS = state.FLAGS ||| FlagMask.A; }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.A; }

// Set C flag based on addition result (flag is set is addition overflows)
let flagC (previousValue: byte) (value: byte) state =
    if (value < previousValue)
    then { state with FLAGS = state.FLAGS ||| FlagMask.C; }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.C; }

// Set S, Z, A, and P flags based on value
let flagSZAP (value: byte) state =
    flagS value state
    |> flagZ value
    |> flagP value
    |> flagA value