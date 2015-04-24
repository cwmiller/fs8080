module Fs8080.Tests.JumpInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers
open Fs8080.JumpInstructions

let internal defaultState = {
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
    WC = 0;
}

// RNZ
[<Test>]
let ``RNZ while stack points to 0xBEEF and Z flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rnz state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RNZ while stack points to 0xBEEF and Z flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.Z }
    |> fun state -> rnz state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)

// JNZ
[<Test>]
let ``JNZ 0xBEEF while Z is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    jnz defaultState memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JNZ 0xBEEF while Z is set should only incease PC by 3`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    jnz defaultState memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

// JMP
[<Test>]
let ``JMP 0xBEEF should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    jmp defaultState memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

// CNZ
[<Test>]
let ``CNZ 0xBEEF while Z flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xC4uy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cnz state memory

    newState.PC.Value
    |> should equal 0xBEEF

    newState.SP.Value
    |> should equal 0xFFFD
    
    changes.Length
    |> should equal 2 

    changes.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFEuy }

    changes.Head
    |> snd
    |> should equal 0xAD

    changes.Tail.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFDuy; }

    changes.Tail.Head
    |> snd
    |> should equal 0xAA

[<Test>]
let ``CNZ 0xBEEF while Z flag is set should only increment PC`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xC4uy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun state -> cnz state memory

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RZ
[<Test>]
let ``RZ while stack points to 0xBEEF and Z flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.Z }
    |> fun state -> rz state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RZ while stack points to 0xBEEF and Z flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rz state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)


// RET
[<Test>]
let ``RET while stack points to 0xBEEF should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> ret state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

// JZ
[<Test>]
let ``JZ 0xBEEF while Z is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    { defaultState with FLAGS = FlagMask.Z }
    |> fun state -> jz state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JZ 0xBEEF while Z is not set should only incease PC by 3`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    jz defaultState memory
    |> fun state -> should equal 0x03 (state.PC.Value)

// CZ
[<Test>]
let ``CZ 0xBEEF while Z flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xC4uy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun state -> cz state memory

    newState.PC.Value
    |> should equal 0xBEEF

    newState.SP.Value
    |> should equal 0xFFFD
    
    changes.Length
    |> should equal 2 

    changes.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFEuy }

    changes.Head
    |> snd
    |> should equal 0xAD

    changes.Tail.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFDuy; }

    changes.Tail.Head
    |> snd
    |> should equal 0xAA

[<Test>]
let ``CZ 0xBEEF while Z flag is not set should only increment PC`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xC4uy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cz state memory

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// CALL
[<Test>]
let ``CALL 0xBEEF should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xCDuy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> call state memory

    newState.PC.Value
    |> should equal 0xBEEF

    newState.SP.Value
    |> should equal 0xFFFD
    
    changes.Length
    |> should equal 2 

    changes.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFEuy; }

    changes.Head
    |> snd
    |> should equal 0xAD

    changes.Tail.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFDuy; }

    changes.Tail.Head
    |> snd
    |> should equal 0xAA

// RNC
[<Test>]
let ``RNC while stack points to 0xBEEF and C flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rnc state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RNC while stack points to 0xBEEF and C flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.C }
    |> fun state -> rnc state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)

// JNC
[<Test>]
let ``JNC 0xBEEF while C is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    jnc defaultState memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JNC 0xBEEF while C is set should only incease PC by 3`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    jnc { defaultState with FLAGS = FlagMask.C } memory
    |> fun state -> should equal 0x03 (state.PC.Value)

// CNC
[<Test>]
let ``CNC 0xBEEF while C flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xC4uy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cnc state memory

    newState.PC.Value
    |> should equal 0xBEEF

    newState.SP.Value
    |> should equal 0xFFFD
    
    changes.Length
    |> should equal 2 

    changes.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFEuy }

    changes.Head
    |> snd
    |> should equal 0xAD

    changes.Tail.Head
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFDuy; }

    changes.Tail.Head
    |> snd
    |> should equal 0xAA

[<Test>]
let ``CNC 0xBEEF while C flag is set should only increment PC`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xC4uy
    Array.set memory 0xAAAB 0xEFuy
    Array.set memory 0xAAAC 0xBEuy

    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun state -> cnc state memory

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0