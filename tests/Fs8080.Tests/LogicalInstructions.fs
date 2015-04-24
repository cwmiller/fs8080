module Fs8080.Tests.LogicalInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers
open Fs8080.LogicalInstructions

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

// INX
[<Test>]
let ``INX B while BC contains 0x00FF should increment BC to 0x100`` () =
    { defaultState with B = 0x00uy; C = 0xFFuy; }
    |> inx BC
    |> get16 BC
    |> should equal { High = 0x01uy; Low = 0x00uy; }

// INR
[<Test>]
let ``INR B while B contains 0x0F should increment B to 0x10`` () =
    { defaultState with B = 0x0Fuy; }
    |> inr B
    |> get8 B
    |> should equal 0x10uy

// DCR
[<Test>]
let ``DCR B while B contains 0x0F should decrement B to 0x0E`` () =
    { defaultState with B = 0x0Fuy; }
    |> dcr B
    |> get8 B
    |> should equal 0x0Euy

// DAD
[<Test>]
let ``DAD D while HL contains 0xDEAD and DE contains 0xBEEF should set HL to 0x9D9C and set the C flag`` () = 
    let newState =
        { defaultState with H = 0xDEuy; L = 0xADuy; D = 0xBEuy; E = 0xEFuy; }
        |> dad DE

    newState
    |> get16 HL
    |> should equal { High = 0x9Duy; Low = 0x9Cuy }

    newState.FLAGS &&& FlagMask.C
    |> should equal FlagMask.C
        
[<Test>]
let ``DAD D while HL contains 0xAABB and DE contains 0x1122 should set HL to 0xBBDD and not set the C flag`` () = 
    let newState =
        { defaultState with H = 0xAAuy; L = 0xBBuy; D = 0x11uy; E = 0x22uy; }
        |> dad DE

    newState
    |> get16 HL
    |> should equal { High = 0xBBuy; Low = 0xDDuy }

    newState.FLAGS &&& FlagMask.C
    |> should not' (equal FlagMask.C)

// DCX
[<Test>]
let ``DCX D when DE contains value 0xFFFF should set the value to 0xFFFE`` () =
    { defaultState with D = 0xFFuy; E = 0xFFuy; }
    |> dcx DE
    |> get16 DE
    |> should equal { High = 0xFFuy; Low = 0xFEuy }

// RRC
[<Test>]
let ``RRC while A contains 0x01 should shift the value to to 0x80 and set the C flag`` () =
    let newState =
        { defaultState with A = 0x01uy }
        |> rrc

    newState.A
    |> should equal 0x80

    newState.FLAGS &&& FlagMask.C
    |> should equal FlagMask.C

[<Test>]
let ``RRC while A contains 0x80 should shift the value to to 0x40 and not set the C flag`` () =
    let newState =
        { defaultState with A = 0x80uy }
        |> rrc

    newState.A
    |> should equal 0x40

    newState.FLAGS &&& FlagMask.C
    |> should not' (equal FlagMask.C)

// RLC
[<Test>]
let ``RLC while A contains 0x40 should set A to 0x80 and not set the C flag`` () =
    let newState = 
        { defaultState with A = 0x40uy }
        |> rlc

    newState
    |> get8 A
    |> should equal 0x80

    newState.FLAGS &&& FlagMask.C
    |> should not' (equal FlagMask.C)

[<Test>]
let ``RLC while A contains 0x80 should set A to 0x00 and set the C flag`` () =
    let newState = 
        { defaultState with A = 0x80uy }
        |> rlc

    newState
    |> get8 A
    |> should equal 0x00

    newState.FLAGS &&& FlagMask.C
    |> should equal FlagMask.C

// RAL
[<Test>]
let ``RAL while A contains 0x01 and the C flag is set should set A to 0x3 and unset the C flag`` () =
    let newState =
        { defaultState with A = 0x01uy; FLAGS = FlagMask.C }
        |> ral

    newState
    |> get8 A
    |> should equal 0x3uy

    newState
    |> get8 FLAGS
    |> should not' (equal FlagMask.C)

[<Test>]
let ``RAL while A contains 0x81 and the C flag is not set should set A to 0x2 and set the C flag`` () =
    let newState =
        { defaultState with A = 0x81uy }
        |> ral

    newState
    |> get8 A
    |> should equal 0x2uy

    newState
    |> get8 FLAGS
    |> should equal FlagMask.C

// RAR
[<Test>]
let ``RAR while A contains 0x81 should set A to 0xC0 and set the C flag`` () =
    let newState =
        { defaultState with A = 0x81uy }
        |> rar

    newState
    |> get8 A
    |> should equal 0xC0uy

    newState
    |> get8 FLAGS
    |> should equal FlagMask.C

[<Test>]
let ``RAR while A contains 0x40 should set A to 0x20 and not set the C flag`` () =
    let newState =
        { defaultState with A = 0x40uy }
        |> rar

    newState
    |> get8 A
    |> should equal 0x20uy

    newState
    |> get8 FLAGS
    |> should not' (equal FlagMask.C)

// DAA
[<Test>]
let ``DAA should only affect PC and WC`` () =
    defaultState
    |> daa
    |> fun state -> (state.A + state.B + state.C + state.D + state.E + state.FLAGS)
    |> should equal 0

// CMA
[<Test>]
let ``CMA while A contains 0xAA should change A to 0x55`` () =
    { defaultState with A = 0xAAuy }
    |> cma
    |> get8 A
    |> should equal 0x55uy

// DCR
[<Test>]
let ``DCR M while HL contains 0xBEEF and memory address 0xBEEF contains 0xDE should set 0xBEEF to 0xDD`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xBEEF 0xDEuy

    let changes = 
        { defaultState with H = 0xBEuy; L = 0xEFuy; }
        |> fun state -> dcr_m state memory
        |> fun (_, changes) -> changes

    changes.Length
    |> should equal 1

    changes.Head
    |> fst
    |> should equal { High = 0xBEuy; Low = 0xEFuy; }

    changes.Head
    |> snd
    |> should equal 0xDD

// STC
[<Test>]
let ``STC should only affect C flag`` () =
    defaultState
    |> stc
    |> get8 FLAGS
    |> should equal FlagMask.C

// CMC
[<Test>]
let ``CMC while FLAGS is 0xD7 should set FLAGS to 0xD6`` () =
    { defaultState with FLAGS = 0xD7uy }
    |> cmc
    |> get8 FLAGS
    |> should equal 0xD6

[<Test>]
let ``CMC while FLAGS is 0x00 should set FLAGS to 0x01`` () =
    { defaultState with FLAGS = 0x00uy }
    |> cmc
    |> get8 FLAGS
    |> should equal 0x01

// INR
[<Test>]
let ``INR M while HL contains 0xBEEF and memory address 0xBEEF contains 0xDE should set 0xBEEF to 0xDF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xBEEF 0xDEuy

    let changes = 
        { defaultState with H = 0xBEuy; L = 0xEFuy; }
        |> fun state -> inr_m state memory
        |> fun (_, changes) -> changes

    changes.Length
    |> should equal 1

    changes.Head
    |> fst
    |> should equal { High = 0xBEuy; Low = 0xEFuy; }

    changes.Head
    |> snd
    |> should equal 0xDF

// ADD
[<Test>]
let ``ADD B while A contains 0xAA and B contains 0x11 should set A to 0xBB and not set C flag`` () =
    let newState = 
        { defaultState with A = 0xAAuy; B = 0x11uy }
        |> add B

    newState.A
    |> should equal 0xBB

    (newState.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``ADD B while A contains 0xAA and B contains 0xCC should set A to 0x76 and set the C flag`` () =
    let newState = 
        { defaultState with A = 0xAAuy; B = 0xCCuy }
        |> add B

    newState.A
    |> should equal 0x76

    (newState.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// ADC
[<Test>]
let ``ADC B while A contains 0xAA, B contains 0x11, and C not set should set A to 0xBB and not set C flag`` () =
    let newState = 
        { defaultState with A = 0xAAuy; B = 0x11uy }
        |> adc B

    newState.A
    |> should equal 0xBB

    (newState.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``ADC B while A contains 0xAA, B contains 0xCC, and C not set should set A to 0x76 and set the C flag`` () =
    let newState = 
        { defaultState with A = 0xAAuy; B = 0xCCuy }
        |> adc B

    newState.A
    |> should equal 0x76

    (newState.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

[<Test>]
let ``ADC B while A contains 0xAA, B contains 0x11, and C is set should set A to 0xBC and not set C flag`` () =
    let newState = 
        { defaultState with A = 0xAAuy; B = 0x11uy; FLAGS = FlagMask.C }
        |> adc B

    newState.A
    |> should equal 0xBC

    (newState.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``ADC B while A contains 0xAA, B contains 0xCC, and C is set should set A to 0x77 and set the C flag`` () =
    let newState = 
        { defaultState with A = 0xAAuy; B = 0xCCuy; FLAGS = FlagMask.C }
        |> adc B

    newState.A
    |> should equal 0x77

    (newState.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// ANI
[<Test>]
let ``ANI 0xAA while A contains 0x0F should set A to 0x0A`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 1 0xAAuy

    { defaultState with A = 0x0Fuy }
    |> fun state -> ani state memory
    |> get8 A
    |> should equal 0x0A