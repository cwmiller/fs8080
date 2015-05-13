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
    WC = 0;
    InterruptsEnabled = false;
    State = State.Running;
}

// RNZ
[<Test>]
let ``RNZ while stack points to 0xBEEF and Z flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rnz cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RNZ while stack points to 0xBEEF and Z flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.Z }
    |> fun cpu -> rnz cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)

// JNZ
[<Test>]
let ``JNZ 0xBEEF while Z is not set should set PC to 0xBEEF`` () =
    jnz { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JNZ 0xBEEF while Z is set should only incease PC by 3`` () =
    jnz { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

// JMP
[<Test>]
let ``JMP 0xBEEF should set PC to 0xBEEF`` () =
    jmp { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

// CNZ
[<Test>]
let ``CNZ 0xBEEF while Z flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cnz { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun cpu -> cnz { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RZ
[<Test>]
let ``RZ while stack points to 0xBEEF and Z flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.Z }
    |> fun cpu -> rz cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RZ while stack points to 0xBEEF and Z flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rz cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)


// RET
[<Test>]
let ``RET while stack points to 0xBEEF should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> ret cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

// JZ
[<Test>]
let ``JZ 0xBEEF while Z is set should set PC to 0xBEEF`` () =
    { defaultCpu with FLAGS = FlagMask.Z }
    |> fun cpu -> jz { High = 0xBEuy; Low = 0xEFuy } cpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JZ 0xBEEF while Z is not set should only incease PC by 3`` () =
    jz { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CZ
[<Test>]
let ``CZ 0xBEEF while Z flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.Z }
        |> fun cpu -> cz { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cz { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// CALL
[<Test>]
let ``CALL 0xBEEF should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> call { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rnc cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RNC while stack points to 0xBEEF and C flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.C }
    |> fun cpu -> rnc cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)

// JNC
[<Test>]
let ``JNC 0xBEEF while C is not set should set PC to 0xBEEF`` () =
    jnc { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JNC 0xBEEF while C is set should only incease PC by 3`` () =
    jnc { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.C }
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CNC
[<Test>]
let ``CNC 0xBEEF while C flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cnc { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun cpu -> cnc { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RC
[<Test>]
let ``RC while stack points to 0xBEEF and C flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.C }
    |> fun cpu -> rc cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

// JC
[<Test>]
let ``JC 0xBEEF while C is set should set PC to 0xBEEF`` () =
    jc { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.C }
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JC 0xBEEF while C is not set should only incease PC by 3`` () =
    jc { High = 0xBEuy; Low = 0xEFuy }defaultCpu
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CC
[<Test>]
let ``CC 0xBEEF while C flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.C }
        |> fun cpu -> cc { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cc { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RPO
[<Test>]
let ``RPO while stack points to 0xBEEF and P flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rpo cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RPO while stack points to 0xBEEF and P flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.P }
    |> fun cpu -> rpo cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)

// JPO
[<Test>]
let ``JPO 0xBEEF while P is not set should set PC to 0xBEEF`` () =
    jpo { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JPO 0xBEEF while P is set should only incease PC by 3`` () =
    jpo { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.P }
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CPO
[<Test>]
let ``CPO 0xBEEF while P flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cpo { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.P }
        |> fun cpu -> cpo { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RPE
[<Test>]
let ``RPE while stack points to 0xBEEF and P flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.P }
    |> fun cpu -> rpe cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RPE while stack points to 0xBEEF and P flag is not set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rpe cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)

// JPE
[<Test>]
let ``JPE 0xBEEF while P is set should set PC to 0xBEEF`` () =
    jpe { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.P }
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JPE 0xBEEF while P is not set should only incease PC by 3`` () =
    jpe { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CPE
[<Test>]
let ``CPE 0xBEEF while P flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; };  FLAGS = FlagMask.P }
        |> fun cpu -> cpe { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cpe { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RP
[<Test>]
let ``RP while stack points to 0xBEEF and S flag is not set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rp cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RP while stack points to 0xBEEF and S flag is set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.S }
    |> fun cpu -> rp cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)

// JP
[<Test>]
let ``JP 0xBEEF while S is not set should set PC to 0xBEEF`` () =
    jp { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JP 0xBEEF while S is set should only incease PC by 3`` () =
    jp { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.S }
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CP
[<Test>]
let ``CP 0xBEEF while S flag is not set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cp { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.S  }
        |> fun cpu -> cp { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0

// RM
[<Test>]
let ``RM while stack points to 0xBEEF and S flag is set should set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy }; FLAGS = FlagMask.S }
    |> fun cpu -> rm cpu memory
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``RM while stack points to 0xBEEF and S flag is not set should not set PC to 0xBEEF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xAAAA 0xEFuy
    Array.set memory 0xAAAB 0xBEuy

    { defaultCpu with SP = { High = 0xAAuy; Low = 0xAAuy } }
    |> fun cpu -> rm cpu memory
    |> fun cpu -> should not' (equal 0xBEEF) (cpu.PC.Value)

// JM
[<Test>]
let ``JM 0xBEEF while S is set should set PC to 0xBEEF`` () =
    jm { High = 0xBEuy; Low = 0xEFuy } { defaultCpu with FLAGS = FlagMask.S }
    |> fun cpu -> should equal 0xBEEF (cpu.PC.Value)

[<Test>]
let ``JM 0xBEEF while S is not set should only incease PC by 3`` () =
    jm { High = 0xBEuy; Low = 0xEFuy } defaultCpu
    |> fun cpu -> should equal 0x03 (cpu.PC.Value)

// CM
[<Test>]
let ``CM 0xBEEF while S flag is set should push PC+3 to the stack then set PC to 0xBEEF`` () =
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; }; FLAGS = FlagMask.S }
        |> fun cpu -> cm { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xBEEF

    newCpu.SP.Value
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
    let (newCpu, changes) =
        { defaultCpu with SP = { High = 0xFFuy; Low = 0xFFuy; }; PC = { High = 0xAAuy; Low = 0xAAuy; } }
        |> fun cpu -> cm { High = 0xBEuy; Low = 0xEFuy } cpu

    newCpu.PC.Value
    |> should equal 0xAAAD

    newCpu.SP.Value
    |> should equal 0xFFFF
    
    changes.Length
    |> should equal 0