module Fs8080.Tests.JumpInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers
open Fs8080.JumpInstructions

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
    INTE = false;
    State = State.Running;
}

// RNZ
[<Test>]
let ``RNZ while stack points to 0xBEEF and Z flag is not set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rnz cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RNZ while stack points to 0xBEEF and Z flag is set should not set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.Z }
    |> fun cpu -> rnz cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)

// JNZ
[<Test>]
let ``JNZ 0xBEEF while Z is not set should set PC to 0xBEEF`` () =
    jnz { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JNZ 0xBEEF while Z is set should only incease PC by 3`` () =
    jnz { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

// JMP
[<Test>]
let ``JMP 0xBEEF should set PC to 0xBEEF`` () =
    jmp { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

// CNZ
[<Test>]
let ``CNZ 0xBEEF while Z flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cnz { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CNZ 0xBEEF while Z flag is set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun cpu -> cnz { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// RST
[<Test>]
let ``RST 7 should push current PC to stack and set PC to 0x38``() =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> rst 7uy cpu Map.empty

    cpu.PC.Value |> should equal 0x38

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAA

// RZ
[<Test>]
let ``RZ while stack points to 0xBEEF and Z flag is set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.Z }
    |> fun cpu -> rz cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RZ while stack points to 0xBEEF and Z flag is not set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rz cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)


// RET
[<Test>]
let ``RET while stack points to 0xBEEF should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> ret cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

// JZ
[<Test>]
let ``JZ 0xBEEF while Z is set should set PC to 0xBEEF`` () =
    { defaultCpu with FLAGS = FlagMask.Z }
    |> fun cpu -> jz { High = 0xBEuy; Low = 0xEFuy } cpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JZ 0xBEEF while Z is not set should only incease PC by 3`` () =
    jz { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03
    
// CZ
[<Test>]
let ``CZ 0xBEEF while Z flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun cpu -> cz { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CZ 0xBEEF while Z flag is not set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cz { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// CALL
[<Test>]
let ``CALL 0xBEEF should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> call { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

// RNC
[<Test>]
let ``RNC while stack points to 0xBEEF and C flag is not set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rnc cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RNC while stack points to 0xBEEF and C flag is set should not set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.C }
    |> fun cpu -> rnc cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)

// JNC
[<Test>]
let ``JNC 0xBEEF while C is not set should set PC to 0xBEEF`` () =
    jnc { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JNC 0xBEEF while C is set should only incease PC by 3`` () =
    jnc { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.C }
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03

// CNC
[<Test>]
let ``CNC 0xBEEF while C flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cnc { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CNC 0xBEEF while C flag is set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun cpu -> cnc { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// RC
[<Test>]
let ``RC while stack points to 0xBEEF and C flag is set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.C }
    |> fun cpu -> rc cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

// JC
[<Test>]
let ``JC 0xBEEF while C is set should set PC to 0xBEEF`` () =
    jc { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.C }
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JC 0xBEEF while C is not set should only incease PC by 3`` () =
    jc { High = 0xBEuy; Low = 0xEFuy }defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03

// CC
[<Test>]
let ``CC 0xBEEF while C flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun cpu -> cc { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CC 0xBEEF while C flag is set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cc { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// RPO
[<Test>]
let ``RPO while stack points to 0xBEEF and P flag is not set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rpo cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RPO while stack points to 0xBEEF and P flag is set should not set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.P }
    |> fun cpu -> rpo cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)

// JPO
[<Test>]
let ``JPO 0xBEEF while P is not set should set PC to 0xBEEF`` () =
    jpo { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JPO 0xBEEF while P is set should only incease PC by 3`` () =
    jpo { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.P }
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03

// CPO
[<Test>]
let ``CPO 0xBEEF while P flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cpo { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CPO 0xBEEF while P flag is set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.P }
        |> fun cpu -> cpo { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// RPE
[<Test>]
let ``RPE while stack points to 0xBEEF and P flag is set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.P }
    |> fun cpu -> rpe cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RPE while stack points to 0xBEEF and P flag is not set should not set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rpe cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)

// JPE
[<Test>]
let ``JPE 0xBEEF while P is set should set PC to 0xBEEF`` () =
    jpe { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.P }
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JPE 0xBEEF while P is not set should only incease PC by 3`` () =
    jpe { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03

// CPE
[<Test>]
let ``CPE 0xBEEF while P flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; };  FLAGS = FlagMask.P }
        |> fun cpu -> cpe { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CPE 0xBEEF while P flag is not set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cpe { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// RP
[<Test>]
let ``RP while stack points to 0xBEEF and S flag is not set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rp cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RP while stack points to 0xBEEF and S flag is set should not set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.S }
    |> fun cpu -> rp cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)

// JP
[<Test>]
let ``JP 0xBEEF while S is not set should set PC to 0xBEEF`` () =
    jp { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JP 0xBEEF while S is set should only incease PC by 3`` () =
    jp { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.S }
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03

// CP
[<Test>]
let ``CP 0xBEEF while S flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cp { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CP 0xBEEF while S flag is set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.S  }
        |> fun cpu -> cp { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF

// RM
[<Test>]
let ``RM while stack points to 0xBEEF and S flag is set should set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.S }
    |> fun cpu -> rm cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``RM while stack points to 0xBEEF and S flag is not set should not set PC to 0xBEEF`` () =
    let memory = [ (0xAAAAus, 0xEFuy); (0xAAABus, 0xBEuy) ] |> Map.ofSeq

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rm cpu memory
    |> fun (cpu, _) -> cpu.PC.Value |> should not' (equal 0xBEEF)

// JM
[<Test>]
let ``JM 0xBEEF while S is set should set PC to 0xBEEF`` () =
    jm { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.S }
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0xBEEF

[<Test>]
let ``JM 0xBEEF while S is not set should only incease PC by 3`` () =
    jm { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun (cpu, _) -> cpu.PC.Value |> should equal 0x03

// CM
[<Test>]
let ``CM 0xBEEF while S flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (cpu, memory, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.S }
        |> fun cpu -> cm { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xBEEF

    cpu.SP.Value |> should equal 0xFFFD

    memory.Item 0xFFFEus |> should equal 0xAA

    memory.Item 0xFFFDus |> should equal 0xAD

[<Test>]
let ``CM 0xBEEF while S flag is not set should only increment PC`` () =
    let (cpu, _, _) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cm { High = 0xBEuy; Low = 0xEFuy } cpu Map.empty

    cpu.PC.Value |> should equal 0xAAAD

    cpu.SP.Value |> should equal 0xFFFF