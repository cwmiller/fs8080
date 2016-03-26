module Fs8080.Tests.Registers

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers

let defaultCpu = {
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

[<Test>]
let ``Setting 16 bit registers should affect 8 bit registers`` () =
    let cpu = set16 RegBC { High = 0xAAuy; Low = 0xBBuy; } defaultCpu

    get8 RegB cpu
    |> should equal 0xAAuy

    get8 RegC cpu
    |> should equal 0xBBuy

[<Test>]
let ``Setting 8 bit registers should affect 16 bit registers`` () =
    defaultCpu
    |> set8 RegB 0xAAuy
    |> set8 RegC 0xBBuy
    |> get16 RegBC
    |> should equal { High = 0xAAuy; Low = 0xBBuy; } 
    
[<Test>]
let ``S flag should be set for an arithmetic result of 128`` () =
    defaultCpu
    |> flagS 128uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.S
    |> should equal FlagMask.S

[<Test>]
let ``S flag should not be set for an arithmetic result of 64`` () =
    defaultCpu
    |> flagS 64uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.S
    |> should not' (equal FlagMask.S)

[<Test>]
let ``Z flag should be set for an arithmetic result of 0`` () =
    defaultCpu
    |> flagZ 0uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.Z
    |> should equal FlagMask.Z

[<Test>]
let ``Z flag should not be set for an arithmetic result of 1`` () =
    defaultCpu
    |> flagZ 1uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.Z
    |> should not' (equal FlagMask.Z)

[<Test>]
let ``P flag should be set for an arithmetic result of 0`` () =
    defaultCpu
    |> flagP 0uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.P
    |> should equal FlagMask.P

[<Test>]
let ``P flag should not be set for an arithmetic result of 191`` () =
    defaultCpu
    |> flagP 191uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.P
    |> should not' (equal FlagMask.P)

[<Test>]
let ``P flag should be set for an arithmetic result of 105`` () =
    defaultCpu
    |> flagP 105uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.P
    |> should equal FlagMask.P

[<Test>]
let ``A flag should be set for an arithmetic result of 0`` () =
    defaultCpu
    |> flagA 0uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.A
    |> should equal FlagMask.A

[<Test>]
let ``A flag should be set for an arithmetic result of 240`` () =
    defaultCpu
    |> flagA 240uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.A
    |> should equal FlagMask.A

[<Test>]
let ``A flag should not be set for an arithmetic result of 242`` () =
    defaultCpu
    |> flagA 242uy
    |> fun cpu -> cpu.FLAGS &&& FlagMask.A
    |> should not' (equal FlagMask.A)