module Fs8080.Tests.MoveInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory
open Fs8080.MoveInstructions

let internal defaultCpu = {
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
    State = State.Running;
}

// LXI
[<Test>]
let ``LXI B, 0xFFF0 should load 0xFF into B and 0xF0 into C`` () =
    let cpu = lxi BC { High = 0xFFuy; Low = 0xF0uy } defaultCpu

    cpu.B |> should equal 0xFF

    cpu.C |> should equal 0xF0

// SHLD
[<Test>]
let ``SHLD 0xBEEF while HL contains 0xDEAD should copy 0xAD to address 0xBEEF and 0xDE to address 0xBEF0`` () =
    let (_, memory) =
        { defaultCpu with H = 0xDEuy; L = 0xADuy }
        |> fun cpu -> shld { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    memory.Item 0xBEEFus |> should equal 0xAD

    memory.Item 0xBEF0us |> should equal 0xDE

// LHLD
[<Test>]
let ``LHLD 0xDEAD while memory address 0xDEAD contains 0xEF and 0xDEAE contains 0xBE should set H to 0xBE and L to 0xEF`` () =
    let cpu =
        [ (1us, 0xADuy); (2us, 0xDEuy); (0xDEADus, 0xEFuy); (0xDEAEus, 0xBEuy) ] 
        |> Map.ofSeq
        |> lhld { High = 0xDEuy; Low = 0xADuy } defaultCpu

    cpu.H |> should equal 0xBE

    cpu.L |> should equal 0xEF

// STAX
[<Test>]
let ``STAX B while A contains 0xFE and BC contains 0xAABB with should place value from A (0xFE) into memory address 0xAABB`` () =
    { defaultCpu with A = 0xFEuy; B = 0xAAuy; C = 0xBBuy; }
    |> fun cpu -> stax BC cpu Map.empty
    |> fun (_, memory) -> memory.Item 0xAABBus 
    |> should equal 0xFE

// MVI
[<Test>]
let ``MVI B, 255 should set B to 0xFF`` () =
    mvi B 0xFFuy defaultCpu
    |> get8 B
    |> should equal 0xFFuy

// LDAX
[<Test>]
let ``LDAX D when DE contains memory address 0xBEEF, containing the value 0xCC, should set A to 0xCC`` () =
    { defaultCpu with D = 0xBEuy; E = 0xEFuy }
    |> fun cpu -> ldax DE cpu <| Map.empty.Add(0xBEEFus, 0xCCuy)
    |> fun cpu -> cpu.A 
    |> should equal 0xCC

// STA
[<Test>]
let ``STA 0xBEEF while A contains 0xFF should set memory address 0xBEEF to 0xFF`` () =
    let memory = [ (0x1us, 0xEFuy); (0x1us, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with A = 0xFFuy }
    |> fun cpu -> sta { High = 0xBEuy; Low = 0xEFuy } cpu memory
    |> fun (_, memory) -> memory.Item 0xBEEFus
    |> should equal 0xFF

// MVI
[<Test>]
let ``MVI M, 0xCC while HL contains 0xBEEF should set 0xBEEF to 0xCC`` () =
    let memory = [ (0x0us, 0x36uy); (0x1us, 0xCCuy) ] |> Map.ofSeq

    { defaultCpu with H = 0xBEuy; L = 0xEFuy; }
    |> fun cpu -> mvi_m 0xCCuy cpu memory
    |> fun (_, memory) -> memory.Item 0xBEEFus
    |> should equal 0xCC

// LDA
[<Test>]
let ``LDA 0xBEEF while 0xBEEF contains 0xAB should set A to 0xAB`` () =
    let memory = [ (0x1us, 0xEFuy); (0x2us, 0xBEuy); (0xBEEFus, 0xABuy) ] |> Map.ofSeq

    lda { High = 0xBEuy; Low = 0xEFuy } defaultCpu memory
    |> get8 A
    |> should equal 0xAB

// MOV
[<Test>]
let ``MOV B, C while C contains 0xAA should set B to 0xAA`` () =
    { defaultCpu with C = 0xAAuy; }
    |> mov_r_r B C
    |> get8 B
    |> should equal 0xAA

[<Test>]
let ``MOV C, M while HL contains address 0xDEAD, containing value 0xBE, should set C to 0xBE`` () =
    { defaultCpu with H = 0xDEuy; L = 0xADuy }
    |> fun cpu -> mov_r_m C cpu <| Map.empty.Add (0xDEADus, 0xBEuy)
    |> get8 C
    |> should equal 0xBE

[<Test>]
let ``MOV M, C while HL contains address 0xDEAD and C contains 0xBE should set memory address 0xDEAD to 0xBE`` () =
    { defaultCpu with H = 0xDEuy; L = 0xADuy; C = 0xBEuy; }
    |> fun cpu -> mov_m_r C cpu Map.empty
    |> fun (_, memory) -> memory.Item 0xDEADus
    |> should equal 0xBE

// POP
[<Test>]
let ``POP B while SP points to value 0xFF and SP+1 points to value 0xAA should set BC to 0xAAFF`` () =
    let memory = [ (0xBEEFus, 0xFFuy); (0xBEF0us, 0xAAuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xBEuy; Low = 0xEFuy } }
    |> fun cpu -> pop BC cpu memory
    |> fun cpu -> (get16 BC cpu).Value
    |> should equal 0xAAFF

[<Test>]
let ``POP B while SP equals 0xBEEF should set SP to 0xBEF1`` () =
    { defaultCpu with SP = { High = 0xBEuy; Low = 0xEFuy }}
    |> fun cpu -> pop BC cpu <| Map.empty.Add (0xBEEFus, 0xFFuy)
    |> fun cpu -> (get16 SP cpu).Value
    |> should equal 0xBEF1

// PUSH
[<Test>]
let ``PUSH B while B contains 0xBE and C contains 0xEF should set SP-2 to 0xEF and SP-1 to 0xBE`` () =
    let (_, memory) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy }; B = 0xBEuy; C = 0xEFuy }
        |> fun cpu -> push BC cpu Map.empty

    fetch 0xFFFDus memory |> should equal 0xEF

    fetch 0xFFFEus memory |> should equal 0xBE
    
[<Test>]
let ``PUSH B while B should set SP tp SP-2`` () =
    { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy }; B = 0xBEuy; C = 0xEFuy }
    |> fun cpu -> push BC cpu Map.empty
    |> fun (cpu, _) -> (cpu.SP.Value) 
    |> should equal 0xFFFD

[<Test>]
let ``XTHL while SP points to 0xDEAD and HL contains 0xBEEF should set SP to 0xBEEF and HL to 0xDEAD`` () =
    let memory = [ (0us, 0xADuy); (0x1us, 0xDEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0x0uy; Low = 0x0uy; }; H = 0xBEuy; L = 0xEFuy; }
    |> fun cpu -> xthl cpu memory
    |> fun (cpu, _) -> (get16 HL cpu).Value
    |> should equal 0xDEAD

[<Test>]
let ``XCHG while HL contains 0xDEAD and DE contains 0xBEEF should set DE to 0xDEAD and HL to 0xBEEF`` () =
    let cpu =
        defaultCpu
        |> set16 HL { High = 0xDEuy; Low = 0xADuy }
        |> set16 DE { High = 0xBEuy; Low = 0xEFuy }
        |> xchg

    (get16 HL cpu).Value |> should equal 0xBEEF 

    (get16 DE cpu).Value |> should equal 0xDEAD

[<Test>]
let ``POP PSW while SP points to 0xBEEF should set A to 0xBE and FLAGS to 0xEF`` () =
    let memory = [ (0x0us, 0xEFuy); (0x1us, 0xBEuy) ] |> Map.ofSeq
    let cpu = pop_psw defaultCpu memory

    cpu.A |> should equal 0xBE

    cpu.FLAGS |> should equal 0xEF

[<Test>]
let ``PUSH PSW while A is set to 0xBE and FLAGS is set to 0xEF should push 0xBEEF to stack`` () =
    let (_, memory) =
        { defaultCpu with A = 0xBEuy; FLAGS = 0xEFuy; SP = { High = 0x00uy; Low = 0x10uy } }
        |> fun cpu -> push_psw cpu Map.empty

    fetch 0x000Eus memory |> should equal 0xEFuy

    fetch 0x000Fus memory |> should equal 0xBEuy