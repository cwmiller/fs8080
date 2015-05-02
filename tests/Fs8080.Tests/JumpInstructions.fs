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
    InterruptsEnabled = false;
    RunState = RunState.Running;
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
    jnz { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JNZ 0xBEEF while Z is set should only incease PC by 3`` () =
    jnz { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0xBEEF (state.PC.Value)

// JMP
[<Test>]
let ``JMP 0xBEEF should set PC to 0xBEEF`` () =
    jmp { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0xBEEF (state.PC.Value)

// CNZ
[<Test>]
let ``CNZ 0xBEEF while Z flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cnz { High = 0xBEuy; Low = 0xEFuy } state

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
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun state -> cnz { High = 0xBEuy; Low = 0xEFuy } state

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
    { defaultState with FLAGS = FlagMask.Z }
    |> fun state -> jz { High = 0xBEuy; Low = 0xEFuy } state
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JZ 0xBEEF while Z is not set should only incease PC by 3`` () =
    jz { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0x03 (state.PC.Value)

// CZ
[<Test>]
let ``CZ 0xBEEF while Z flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun state -> cz { High = 0xBEuy; Low = 0xEFuy } state

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
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cz { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// CALL
[<Test>]
let ``CALL 0xBEEF should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> call { High = 0xBEuy; Low = 0xEFuy } state

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
    jnc { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JNC 0xBEEF while C is set should only incease PC by 3`` () =
    jnc { High = 0xBEuy; Low = 0xEFuy } { defaultState with FLAGS = FlagMask.C }
    |> fun state -> should equal 0x03 (state.PC.Value)

// CNC
[<Test>]
let ``CNC 0xBEEF while C flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cnc { High = 0xBEuy; Low = 0xEFuy } state

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
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun state -> cnc { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RC
[<Test>]
let ``RC while stack points to 0xBEEF and C flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.C }
    |> fun state -> rc state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

// JC
[<Test>]
let ``JC 0xBEEF while C is set should set PC to 0xBEEF`` () =
    jc { High = 0xBEuy; Low = 0xEFuy } { defaultState with FLAGS = FlagMask.C }
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JC 0xBEEF while C is not set should only incease PC by 3`` () =
    jc { High = 0xBEuy; Low = 0xEFuy }defaultState
    |> fun state -> should equal 0x03 (state.PC.Value)

// CC
[<Test>]
let ``CC 0xBEEF while C flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun state -> cc { High = 0xBEuy; Low = 0xEFuy } state

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
let ``CC 0xBEEF while C flag is set should only increment PC`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cc { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RPO
[<Test>]
let ``RPO while stack points to 0xBEEF and P flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rpo state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RPO while stack points to 0xBEEF and P flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.P }
    |> fun state -> rpo state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)

// JPO
[<Test>]
let ``JPO 0xBEEF while P is not set should set PC to 0xBEEF`` () =
    jpo { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JPO 0xBEEF while P is set should only incease PC by 3`` () =
    jpo { High = 0xBEuy; Low = 0xEFuy } { defaultState with FLAGS = FlagMask.P }
    |> fun state -> should equal 0x03 (state.PC.Value)

// CPO
[<Test>]
let ``CPO 0xBEEF while P flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cpo { High = 0xBEuy; Low = 0xEFuy } state

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
let ``CPO 0xBEEF while P flag is set should only increment PC`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.P }
        |> fun state -> cpo { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RPE
[<Test>]
let ``RPE while stack points to 0xBEEF and P flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.P }
    |> fun state -> rpe state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RPE while stack points to 0xBEEF and P flag is not set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rpe state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)

// JPE
[<Test>]
let ``JPE 0xBEEF while P is set should set PC to 0xBEEF`` () =
    jpe { High = 0xBEuy; Low = 0xEFuy } { defaultState with FLAGS = FlagMask.P }
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JPE 0xBEEF while P is not set should only incease PC by 3`` () =
    jpe { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0x03 (state.PC.Value)

// CPE
[<Test>]
let ``CPE 0xBEEF while P flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; };  FLAGS = FlagMask.P }
        |> fun state -> cpe { High = 0xBEuy; Low = 0xEFuy } state

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
let ``CPE 0xBEEF while P flag is not set should only increment PC`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cpe { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RP
[<Test>]
let ``RP while stack points to 0xBEEF and S flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rp state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RP while stack points to 0xBEEF and S flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.S }
    |> fun state -> rp state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)

// JP
[<Test>]
let ``JP 0xBEEF while S is not set should set PC to 0xBEEF`` () =
    jp { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JP 0xBEEF while S is set should only incease PC by 3`` () =
    jp { High = 0xBEuy; Low = 0xEFuy } { defaultState with FLAGS = FlagMask.S }
    |> fun state -> should equal 0x03 (state.PC.Value)

// CP
[<Test>]
let ``CP 0xBEEF while S flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cp { High = 0xBEuy; Low = 0xEFuy } state

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
let ``CP 0xBEEF while S flag is set should only increment PC`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.S  }
        |> fun state -> cp { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RM
[<Test>]
let ``RM while stack points to 0xBEEF and S flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.S }
    |> fun state -> rm state memory
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``RM while stack points to 0xBEEF and S flag is not set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultState with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun state -> rm state memory
    |> fun state -> should not' (equal 0xBEEF) (state.PC.Value)

// JM
[<Test>]
let ``JM 0xBEEF while S is set should set PC to 0xBEEF`` () =
    jm { High = 0xBEuy; Low = 0xEFuy } { defaultState with FLAGS = FlagMask.S }
    |> fun state -> should equal 0xBEEF (state.PC.Value)

[<Test>]
let ``JM 0xBEEF while S is not set should only incease PC by 3`` () =
    jm { High = 0xBEuy; Low = 0xEFuy } defaultState
    |> fun state -> should equal 0x03 (state.PC.Value)

// CM
[<Test>]
let ``CM 0xBEEF while S flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.S }
        |> fun state -> cm { High = 0xBEuy; Low = 0xEFuy } state

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
let ``CM 0xBEEF while S flag is not set should only increment PC`` () =
    let (newState, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun state -> cm { High = 0xBEuy; Low = 0xEFuy } state

    newState.PC.Value
    |> should equal 0xAAAD

    newState.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0