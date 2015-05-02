module  Fs8080.Types

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

type RunState =
    | Stopped
    | Running
    | Halted

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
    SP: DWord;
    PC: DWord;
    FLAGS: byte;

    // Interrupts enabled or disabled
    InterruptsEnabled: bool;

    // Running state
    RunState: RunState

    // Work cycles
    WC: int;
}