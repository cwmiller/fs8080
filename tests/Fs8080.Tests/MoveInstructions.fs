module Fs8080.Tests.MoveInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers
open Fs8080.MoveInstructions

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

// LXI
[<Test>]
let ``LXI B, 0xFFF0 should load 0xFF into B and 0xF0 into C`` () =
    let newState = lxi BC { High = 0xFFuy; Low = 0xF0uy } defaultState

    newState.B
    |> should equal 0xFF

    newState.C
    |> should equal 0xF0

// SHLD
[<Test>]
let ``SHLD 0xBEEF while HL contains 0xDEAD should copy 0xAD to address 0xBEEF and 0xDE to address 0xBEF0`` () =
    let changes =
        { defaultState with H = 0xDEuy; L = 0xADuy }
        |> fun state -> shld { High = 0xBEuy; Low = 0xEFuy } state
        |> fun (_, changes) -> changes

    changes.Length
    |> should equal 2

    changes.Item 0
    |> fst
    |> should equal { High = 0xBEuy; Low = 0xEFuy; }

    changes.Item 0
    |> snd
    |> should equal 0xADuy;

    changes.Item 1
    |> fst
    |> should equal { High = 0xBEuy; Low = 0xF0uy; }

    changes.Item 1
    |> snd
    |> should equal 0xDEuy;

// LHLD
[<Test>]
let ``LHLD 0xDEAD while memory address 0xDEAD contains 0xEF and 0xDEAE contains 0xBE should set H to 0xBE and L to 0xEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xADuy
    Array.set memory 2 0xDEuy
    Array.set memory 0xDEAD 0xEFuy
    Array.set memory 0xDEAE 0xBEuy

    let newState = lhld { High = 0xDEuy; Low = 0xADuy } defaultState memory

    newState.H
    |> should equal 0xBE

    newState.L
    |> should equal 0xEF

// STAX
[<Test>]
let ``STAX B while A contains 0xFE and BC contains 0xAABB with should place value from A (0xFE) into memory address 0xAABB`` () =
    let changes =
        { defaultState with A = 0xFEuy; B = 0xAAuy; C = 0xBBuy; }
        |> stax BC
        |> fun (_, changes) -> changes

    changes.Length
    |> should equal 1

    changes.Head
    |> fst
    |> should equal { High = 0xAAuy; Low = 0xBBuy; }

    changes.Head
    |> snd
    |> should equal 0xFE

// MVI
[<Test>]
let ``MVI B, 255 should set B to 0xFF`` () =
    defaultState
    |> fun state -> mvi B 0xFFuy state
    |> get8 B
    |> should equal 0xFFuy

// LDAX
[<Test>]
let ``LDAX D when DE contains memory address 0xBEEF, containing the value 0xCC, should set A to 0xCC`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xBEEF 0xCCuy

    { defaultState with D = 0xBEuy; E = 0xEFuy }
    |> fun state -> ldax DE state memory
    |> fun state -> should equal 0xCC (state.A)

// STA
[<Test>]
let ``STA 0xBEEF while A contains 0xFF should set memory address 0xBEEF to 0xFF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xEFuy
    Array.set memory 2 0xBEuy

    let changes = 
        { defaultState with A = 0xFFuy }
        |> fun state -> sta { High = 0xBEuy; Low = 0xEFuy } state
        |> fun (_, changes) -> changes.Head

    changes
    |> fst
    |> should equal { High = 0xBEuy; Low = 0xEFuy; }

    changes
    |> snd
    |> should equal 0xFF

// MVI
[<Test>]
let ``MVI M, 0xCC while HL contains 0xBEEF should set 0xBEEF to 0xCC`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0x0 0x36uy
    Array.set memory 0x1 0xCCuy

    let changes = 
        { defaultState with H = 0xBEuy; L = 0xEFuy; }
        |> fun state -> mvi_m 0xcc state
        |> fun (_, changes) -> changes

    changes.Length
    |> should equal 1

    changes.Head
    |> fst
    |> should equal { High = 0xBEuy; Low = 0xEFuy; }

    changes.Head
    |> snd
    |> should equal 0xCC

// LDA
[<Test>]
let ``LDA 0xBEEF while 0xBEEF contains 0xAB should set A to 0xAB`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0x1 0xEFuy
    Array.set memory 0x2 0xBEuy
    Array.set memory 0xBEEF 0xABuy

    lda { High = 0xBEuy; Low = 0xEFuy } defaultState memory
    |> get8 A
    |> should equal 0xAB

// MOV
[<Test>]
let ``MOV B, C while C contains 0xAA should set B to 0xAA`` () =
    { defaultState with C = 0xAAuy; }
    |> mov_r_r B C
    |> get8 B
    |> should equal 0xAA

[<Test>]
let ``MOV C, M while HL contains address 0xDEAD, containing value 0xBE, should set C to 0xBE`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xDEAD 0xBEuy

    { defaultState with H = 0xDEuy; L = 0xADuy }
    |> fun state -> mov_r_m C state memory
    |> get8 C
    |> should equal 0xBE

[<Test>]
let ``MOV M, C while HL contains address 0xDEAD and C contains 0xBE should set memory address 0xDEAD to 0xBE`` () =
    let changes =
        { defaultState with H = 0xDEuy; L = 0xADuy; C = 0xBEuy; }
        |> mov_m_r C 
        |> fun (_, changes) -> changes
        
    changes.Length
    |> should equal 1

    changes.Head
    |> fst
    |> should equal { High = 0xDEuy; Low = 0xADuy; }

    changes.Head
    |> snd
    |> should equal 0xBE

// POP
[<Test>]
let ``POP B while SP points to value 0xFF and SP+1 points to value 0xAA should set BC to 0xAAFF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xBEEF 0xFFuy
    Array.set memory 0xBEF0 0xAAuy

    { defaultState with SP = { High = 0xBEuy; Low = 0xEFuy }}
    |> fun state -> pop BC state memory
    |> fun state -> get16 BC state
    |> fun v -> should equal 0xAAFF (v.Value)

[<Test>]
let ``POP B while SP equals 0xBEEF should set SP to 0xBEF1`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xBEEF 0xFFuy

    { defaultState with SP = { High = 0xBEuy; Low = 0xEFuy }}
    |> fun state -> pop BC state memory
    |> fun state -> get16 SP state
    |> fun addr -> should equal 0xBEF1 addr.Value

// PUSH
[<Test>]
let ``PUSH B while B contains 0xBE and C contains 0xEF should set SP-2 to 0xEF and SP-1 to 0xBE`` () =
    let (_, changes) =
        { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy }; B = 0xBEuy; C = 0xEFuy }
        |> push BC

    changes.Length
    |> should equal 2

    changes.Item 0
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFDuy; }

    changes.Item 0
    |> snd
    |> should equal 0xEFuy;

    changes.Item 1
    |> fst
    |> should equal { High = 0xFFuy; Low = 0xFEuy; }

    changes.Item 1
    |> snd
    |> should equal 0xBEuy;
    
[<Test>]
let ``PUSH B while B should set SP tp SP-2`` () =
    { defaultState with SP = { High = 0xFFuy; Low = 0xFFuy }; B = 0xBEuy; C = 0xEFuy }
    |> push BC
    |> fun (state, _) -> should equal 0xFFFD (state.SP.Value)

[<Test>]
let ``XTHL while SP points to 0xDEAD and HL contains 0xBEEF should set SP to 0xBEEF and HL to 0xDEAD`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0 0xADuy
    Array.set memory 1 0xDEuy

    { defaultState with SP = { High = 0x0uy; Low = 0x0uy; }; H = 0xBEuy; L = 0xEFuy; }
    |> fun state -> xthl state memory
    |> fun (state, _) -> should equal 0xDEAD (get16 HL state).Value

[<Test>]
let ``XCHG while HL contains 0xDEAD and DE contains 0xBEEF should set DE to 0xDEAD and HL to 0xBEEF`` () =
    let state =
        defaultState
        |> set16 HL { High = 0xDEuy; Low = 0xADuy }
        |> set16 DE { High = 0xBEuy; Low = 0xEFuy }
        |> xchg

    (get16 HL state).Value |> should equal 0xBEEF 
    (get16 DE state).Value |> should equal 0xDEAD
