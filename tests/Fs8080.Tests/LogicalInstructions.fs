module Fs8080.Tests.LogicalInstructions

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers
open Fs8080.LogicalInstructions

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

// INX
[<Test>]
let ``INX B while BC contains 0x00FF should increment BC to 0x100`` () =
    { defaultCpu with B = 0x00uy; C = 0xFFuy; }
    |> inx RegBC
    |> fun (cpu, _) -> get16 RegBC cpu
    |> should equal { High = 0x01uy; Low = 0x00uy; }

// INR
[<Test>]
let ``INR B while B contains 0x0F should increment B to 0x10`` () =
    { defaultCpu with B = 0x0Fuy; }
    |> inr RegB
    |> fun (cpu, _) -> get8 RegB cpu
    |> should equal 0x10uy

// DCR
[<Test>]
let ``DCR B while B contains 0x0F should decrement B to 0x0E`` () =
    { defaultCpu with B = 0x0Fuy; }
    |> dcr RegB
    |> fun (cpu, _) -> get8 RegB cpu
    |> should equal 0x0Euy

// DAA
[<Test>]
let ``DAA while A contains 0x9B and both carry flags are not set should A to 0x01 and set both carry flags``() =
    let (cpu, _) =
        { defaultCpu with A = 0x9Buy; FLAGS = FlagMask.A ||| FlagMask.C }
        |> daa

    should equal 0x1 <| cpu.A
    should equal FlagMask.A <| (cpu.FLAGS &&& FlagMask.A)
    should equal FlagMask.C <| (cpu.FLAGS &&& FlagMask.C)


// DAD
[<Test>]
let ``DAD D while HL contains 0xDEAD and DE contains 0xBEEF should set HL to 0x9D9C and set the C flag`` () = 
    let (cpu, _) =
        { defaultCpu with H = 0xDEuy; L = 0xADuy; D = 0xBEuy; E = 0xEFuy; }
        |> dad RegDE

    cpu
    |> get16 RegHL
    |> should equal { High = 0x9Duy; Low = 0x9Cuy }

    cpu.FLAGS &&& FlagMask.C
    |> should equal FlagMask.C
        
[<Test>]
let ``DAD D while HL contains 0xAABB and DE contains 0x1122 should set HL to 0xBBDD and not set the C flag`` () = 
    let (cpu, _) =
        { defaultCpu with H = 0xAAuy; L = 0xBBuy; D = 0x11uy; E = 0x22uy; }
        |> dad RegDE

    cpu
    |> get16 RegHL
    |> should equal { High = 0xBBuy; Low = 0xDDuy }

    cpu.FLAGS &&& FlagMask.C
    |> should not' (equal FlagMask.C)

// DCX
[<Test>]
let ``DCX D when DE contains value 0xFFFF should set the value to 0xFFFE`` () =
    { defaultCpu with D = 0xFFuy; E = 0xFFuy; }
    |> dcx RegDE
    |> fun (cpu, _) -> get16 RegDE cpu
    |> should equal { High = 0xFFuy; Low = 0xFEuy }

// RRC
[<Test>]
let ``RRC while A contains 0x01 should shift the value to to 0x80 and set the C flag`` () =
    let (cpu, _) =
        { defaultCpu with A = 0x01uy }
        |> rrc

    cpu.A |> should equal 0x80

    cpu.FLAGS &&& FlagMask.C
    |> should equal FlagMask.C

[<Test>]
let ``RRC while A contains 0x80 should shift the value to to 0x40 and not set the C flag`` () =
    let (cpu, _) = rrc { defaultCpu with A = 0x80uy }

    cpu.A |> should equal 0x40

    cpu.FLAGS &&& FlagMask.C
    |> should not' (equal FlagMask.C)

// RLC
[<Test>]
let ``RLC while A contains 0x40 should set A to 0x80 and not set the C flag`` () =
    let (cpu, _) = rlc { defaultCpu with A = 0x40uy }

    cpu.A |> should equal 0x80

    cpu.FLAGS &&& FlagMask.C
    |> should not' (equal FlagMask.C)

[<Test>]
let ``RLC while A contains 0x80 should set A to 0x00 and set the C flag`` () =
    let (cpu, _) = rlc { defaultCpu with A = 0x80uy }

    cpu.A |> should equal 0x00

    cpu.FLAGS &&& FlagMask.C
    |> should equal FlagMask.C

// RAL
[<Test>]
let ``RAL while A contains 0x01 and the C flag is set should set A to 0x3 and unset the C flag`` () =
    let (cpu, _) = ral { defaultCpu with A = 0x01uy; FLAGS = FlagMask.C }

    cpu.A |> should equal 0x3uy

    cpu.FLAGS |> should not' (equal FlagMask.C)

[<Test>]
let ``RAL while A contains 0x81 and the C flag is not set should set A to 0x2 and set the C flag`` () =
    let (cpu, _) = ral { defaultCpu with A = 0x81uy }

    cpu.A |> should equal 0x2uy

    cpu.FLAGS |> should equal FlagMask.C

// RAR
[<Test>]
let ``RAR while A contains 0x81 should set A to 0xC0 and set the C flag`` () =
    let (cpu, _) = rar { defaultCpu with A = 0x81uy }

    cpu.A |> should equal 0xC0uy

    cpu.FLAGS |> should equal FlagMask.C

[<Test>]
let ``RAR while A contains 0x40 should set A to 0x20 and not set the C flag`` () =
    let (cpu, _) = rar { defaultCpu with A = 0x40uy }

    cpu.A |> should equal 0x20uy

    cpu.FLAGS |> should not' (equal FlagMask.C)

// DAA
[<Test>]
let ``DAA should only affect PC and WC`` () =
    defaultCpu
    |> daa
    |> fun (cpu, _) -> (cpu.A + cpu.B + cpu.C + cpu.D + cpu.E + cpu.FLAGS)
    |> should equal 0

// CMA
[<Test>]
let ``CMA while A contains 0xAA should change A to 0x55`` () =
    { defaultCpu with A = 0xAAuy }
    |> cma
    |> fun (cpu, _) -> get8 RegA cpu
    |> should equal 0x55uy

// DCR
[<Test>]
let ``DCR M while HL contains 0xBEEF and memory address 0xBEEF contains 0xDE should set 0xBEEF to 0xDD`` () =
    let (_, memory, _) =
        { defaultCpu with H = 0xBEuy; L = 0xEFuy; }
        |> fun cpu -> dcr_m cpu <| Map.empty.Add(0xBEEFus, 0xDEuy)

    memory.Item 0xBEEFus |> should equal 0xDD

// STC
[<Test>]
let ``STC should only affect C flag`` () =
    defaultCpu
    |> stc
    |> fun (cpu, _) -> get8 RegFLAGS cpu
    |> should equal FlagMask.C

// CMC
[<Test>]
let ``CMC while FLAGS is 0xD7 should set FLAGS to 0xD6`` () =
    { defaultCpu with FLAGS = 0xD7uy }
    |> cmc
    |> fun (cpu, _) -> get8 RegFLAGS cpu
    |> should equal 0xD6

[<Test>]
let ``CMC while FLAGS is 0x00 should set FLAGS to 0x01`` () =
    { defaultCpu with FLAGS = 0x00uy }
    |> cmc
    |> fun (cpu, _) -> get8 RegFLAGS cpu
    |> should equal 0x01

// INR
[<Test>]
let ``INR M while HL contains 0xBEEF and memory address 0xBEEF contains 0xDE should set 0xBEEF to 0xDF`` () =
    let (_, memory, _) =
        { defaultCpu with H = 0xBEuy; L = 0xEFuy; }
        |> fun cpu -> inr_m cpu <| Map.empty.Add(0xBEEFus, 0xDEuy)

    memory.Item 0xBEEFus |> should equal 0xDF

// ADD
[<Test>]
let ``ADD B while A contains 0xAA and B contains 0x11 should set A to 0xBB and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0x11uy }
        |> add RegB

    cpu.A |> should equal 0xBB

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``ADD B while A contains 0xAA and B contains 0xCC should set A to 0x76 and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0xCCuy }
        |> add RegB

    cpu.A |> should equal 0x76

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// ADC
[<Test>]
let ``ADC B while A contains 0xAA, B contains 0x11, and C not set should set A to 0xBB and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0x11uy }
        |> adc RegB

    cpu.A |> should equal 0xBB

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``ADC B while A contains 0xAA, B contains 0xCC, and C not set should set A to 0x76 and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0xCCuy }
        |> adc RegB

    cpu.A |> should equal 0x76

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

[<Test>]
let ``ADC B while A contains 0xAA, B contains 0x11, and C is set should set A to 0xBC and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0x11uy; FLAGS = FlagMask.C }
        |> adc RegB

    cpu.A |> should equal 0xBC

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``ADC B while A contains 0xAA, B contains 0xCC, and C is set should set A to 0x77 and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0xCCuy; FLAGS = FlagMask.C }
        |> adc RegB

    cpu.A |> should equal 0x77

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// SUB
[<Test>]
let ``SUB B while A contains 0xBB and B contains 0x11 should set A to 0xAA and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xBBuy; B = 0x11uy }
        |> sub RegB

    cpu.A |> should equal 0xAA

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``SUB B while A contains 0xAA and B contains 0xCC should set A to 0xDE and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0xCCuy }
        |> sub RegB

    cpu.A |> should equal 0xDE

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// SUB M
[<Test>]
let ``SUB M while A contains 0xBB and HL points to 0x11 should set A to 0xAA and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xBBuy; H = 0xBEuy; L = 0xEFuy }
        |> fun cpu -> sub_m cpu <| Map.empty.Add (0xBEEFus, 0x11uy)

    cpu.A |> should equal 0xAA

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``SUB M while A contains 0xAA and HL points to 0xCC should set A to 0xDE and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; H = 0xBEuy; L = 0xEFuy }
        |> fun cpu -> sub_m cpu <| Map.empty.Add (0xBEEFus, 0xCCuy)

    cpu.A |> should equal 0xDE

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// SBB
[<Test>]
let ``SBB B while A contains 0xAA, B contains 0x11, and C not set should set A to 0x99 and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0x11uy }
        |> sbb RegB

    cpu.A |> should equal 0x99

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``SBB B while A contains 0xAA, B contains 0xCC, and C not set should set A to 0xDE and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0xCCuy }
        |> sbb RegB

    cpu.A |> should equal 0xDE

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

[<Test>]
let ``SBB B while A contains 0xAA, B contains 0x11, and C is set should set A to 0x98 and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0x11uy; FLAGS = FlagMask.C }
        |> sbb RegB

    cpu.A |> should equal 0x98

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``SBB B while A contains 0xAA, B contains 0xCC, and C is set should set A to 0xDD and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; B = 0xCCuy; FLAGS = FlagMask.C }
        |> sbb RegB

    cpu.A |> should equal 0xDD

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// SBB M
[<Test>]
let ``SBB M while A contains 0xAA, HL points to 0x11, and C not set should set A to 0x99 and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; H = 0xBEuy; L = 0xEFuy }
        |> fun cpu -> sbb_m cpu <| Map.empty.Add (0xBEEFus, 0x11uy)

    cpu.A |> should equal 0x99

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``SBB M while A contains 0xAA, HL points to 0xCC, and C not set should set A to 0xDE and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; H = 0xBEuy; L = 0xEFuy  }
        |> fun cpu -> sbb_m cpu <| Map.empty.Add (0xBEEFus, 0xCCuy)

    cpu.A |> should equal 0xDE

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

[<Test>]
let ``SBB M while A contains 0xAA, HL points to 0x11, and C is set should set A to 0x98 and not set C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; H = 0xBEuy; L = 0xEFuy; FLAGS = FlagMask.C }
        |> fun cpu -> sbb_m cpu <| Map.empty.Add (0xBEEFus, 0x11uy)

    cpu.A |> should equal 0x98

    (cpu.FLAGS &&& FlagMask.C)
    |> should not' (equal FlagMask.C)

[<Test>]
let ``SBB M while A contains 0xAA, HL points to 0xCC, and C is set should set A to 0xDD and set the C flag`` () =
    let (cpu, _) = 
        { defaultCpu with A = 0xAAuy; H = 0xBEuy; L = 0xEFuy; FLAGS = FlagMask.C }
        |> fun cpu -> sbb_m cpu <| Map.empty.Add (0xBEEFus, 0xCCuy)

    cpu.A |> should equal 0xDD

    (cpu.FLAGS &&& FlagMask.C)
    |> should equal FlagMask.C

// ANA
[<Test>]
let ``ANA B while A contains 0xF0 and B contains 0x0F should set A to 0x00`` () =
    { defaultCpu with A = 0xF0uy; B = 0x0Fuy }
    |> ana RegB
    |> fun (cpu, _) -> cpu.A |> should equal 0x00

[<Test>]
let ``ANA M while A contains 0xF0 and HL points to 0x0F should set A to 0x00`` () =
    { defaultCpu with A = 0xF0uy; H = 0xBEuy; L = 0xEFuy }
    |> fun cpu -> ana_m cpu <| Map.empty.Add (0xBEEFus, 0x0Fuy)
    |> fun (cpu, _) -> cpu.A |> should equal 0x00

// XRA
[<Test>]
let ``XRA B while A contains 0xF0 and B contains 0x0F should set A to 0xFF`` () =
    { defaultCpu with A = 0xF0uy; B = 0x0Fuy }
    |> xra RegB
    |> fun (cpu, _) -> cpu.A |> should equal 0xFF

[<Test>]
let ``XRA M while A contains 0xF0 and HL points to 0x0F should set A to 0xFF`` () =
    { defaultCpu with A = 0xF0uy; H = 0xBEuy; L = 0xEFuy }
    |> fun cpu -> xra_m cpu <| Map.empty.Add (0xBEEFus, 0x0Fuy)
    |> fun (cpu, _) -> cpu.A |> should equal 0xFF

// ORA
[<Test>]
let ``ORA B while A contains 0xF0 and B contains 0x0F should set A to 0xFF`` () =
    { defaultCpu with A = 0xF0uy; B = 0x0Fuy }
    |> ora RegB
    |> fun (cpu, _) -> cpu.A |> should equal 0xFF

[<Test>]
let ``ORA M while A contains 0xF0 and HL points to 0x0F should set A to 0xFF`` () =
    let memory = Array.zeroCreate<byte> 65535
    Array.set memory 0xBEEF 0x0Fuy

    { defaultCpu with A = 0xF0uy; H = 0xBEuy; L = 0xEFuy }
    |> fun cpu -> ora_m cpu <| Map.empty.Add (0xBEEFus, 0x0Fuy)
    |> fun (cpu, _) -> cpu.A |> should equal 0xFF

// CMP
[<Test>]
let ``CMP B while A contains 0xF0 and B contains 0xF0 should set Z flag`` () =
    { defaultCpu with A = 0xF0uy; B = 0xF0uy }
    |> cmp RegB
    |> fun (cpu , _)-> (cpu.FLAGS &&& FlagMask.Z) |> should equal FlagMask.Z 

[<Test>]
let ``CMP M while A contains 0xF0 and HL points to 0xF0 should set Z flag`` () =
    { defaultCpu with A = 0xF0uy; H = 0xBEuy; L = 0xEFuy }
    |> fun cpu -> cmp_m cpu <| Map.empty.Add (0xBEEFus, 0xF0uy)
    |> fun (cpu, _) -> (cpu.FLAGS &&& FlagMask.Z) |> should equal FlagMask.Z 

// ADI
[<Test>]
let ``ADI 0xCC while A contains 0xDD should set A to 0xA9`` () =
    { defaultCpu with A = 0xDDuy }
    |> adi 0xCCuy
    |> fun (cpu, _) -> cpu.A |> should equal 0xA9

// ACI
[<Test>]
let ``ACI 0xCC while A contains 0xDD and C flag is set should set A to 0xAA`` () =
    { defaultCpu with A = 0xDDuy; FLAGS = FlagMask.C }
    |> aci 0xCCuy
    |> fun (cpu, _) -> cpu.A |> should equal 0xAA

// SUI
[<Test>]
let ``SUI 0xCC while A contains 0xDD should set A to 0x11`` () =
    { defaultCpu with A = 0xDDuy }
    |> sui 0xCCuy
    |> fun (cpu, _) -> cpu.A |> should equal 0x11

// ANI
[<Test>]
let ``ANI 0xAA while A contains 0x0F should set A to 0x0A`` () =
    { defaultCpu with A = 0x0Fuy }
    |> ani 0xAAuy
    |> fun (cpu, _) -> cpu.A |> should equal 0x0A

// XRI
[<Test>]
let ``XRI 0xAA while A contains 0x0F should set A to 0xA5`` () =
    { defaultCpu with A = 0x0Fuy }
    |> xri 0xAAuy
    |> fun (cpu, _) -> cpu.A |> should equal 0xA5

// ORI
[<Test>]
let ``ORI 0xAA while A contains 0x0F should set A to 0xAF`` () =
    { defaultCpu with A = 0x0Fuy }
    |> ori 0xAAuy
    |> fun (cpu, _) -> cpu.A |> should equal 0xAF

// CPI
[<Test>]
let ``CPI 0xAA while A contains 0xAA should set Z flag`` () =
    { defaultCpu with A = 0xAAuy }
    |> cpi 0xAAuy
    |> fun (cpu, _) -> (cpu.FLAGS &&& FlagMask.Z) |> should equal FlagMask.Z