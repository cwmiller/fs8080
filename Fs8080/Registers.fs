module Fs8080.Registers

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

// Represents the CPU's current state
type State = {
    // Registers
    A: byte;
    B: byte;
    C: byte;
    D: byte;
    E: byte;
    H: byte;
    L: byte;
    SP: uint16;
    PC: uint16;
    FLAGS: byte;

    // Work cycles
    WC: int;
}

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
        | AF -> (uint16 state.A <<< 8) ||| (uint16 state.FLAGS)
        | BC -> (uint16 state.B <<< 8) ||| (uint16 state.C)
        | DE -> (uint16 state.D <<< 8) ||| (uint16 state.E)
        | HL -> (uint16 state.H <<< 8) ||| (uint16 state.L)
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

// Gets the value of a 16bit register
let set16 register (value: uint16) state =
    let high = byte (value >>> 8)
    let low = byte value

    match register with
        | AF -> { state with A = high; FLAGS = low; }
        | BC -> { state with B = high; C = low; }
        | DE -> { state with D = high; E = low; }
        | HL -> { state with H = high; L = low; }
        | SP -> { state with SP = (uint16 value); }
        | PC -> { state with PC = (uint16 value); }

// Copy the value from one 8bit register to another
let copy8 src dest state =
    get8 src state
    |> fun value -> set8 dest value state

// Increment PC register
let incPC amt state = 
    { state with PC = state.PC + amt }

// Increment cycle counter
let incWC amt state = 
    { state with WC = state.WC + amt }

// Set S flag based on value
let setS (value: byte) state =
    if (value &&& 0x80uy) > 0uy
    then { state with FLAGS = state.FLAGS ||| FlagMask.S }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.S }

// Set Z flag based on value
let setZ (value: byte) state = 
    if value = 0uy 
    then { state with FLAGS = state.FLAGS ||| FlagMask.Z; }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.Z; }

// Set P flag based on value
let setP (value: byte) state =
    let cnt = 
        [0..7]
        |> List.fold (fun acc idx -> acc + (value >>> idx) &&& 0x1uy) 0uy

    if cnt % 2uy = 0uy
    then { state with FLAGS = state.FLAGS ||| FlagMask.P; }
    else { state with FLAGS = state.FLAGS &&& ~~~FlagMask.P; }

// Set S, Z, and P flags based on value
let setSZP (value: byte) state =
    setS value state
    |> setZ value
    |> setP value