module Fs8080.Tests.Registers

open NUnit.Framework
open FsUnit

open Fs8080.Types
open Fs8080.Registers

let defaultState = {
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

[<Test>]
let ``Setting 16 bit registers should affect 8 bit registers`` () =
    let state = set16 BC { High = 0xAAuy; Low = 0xBBuy; } defaultState

    get8 B state
    |> should equal 0xAAuy

    get8 C state
    |> should equal 0xBBuy

[<Test>]
let ``Setting 8 bit registers should affect 16 bit registers`` () =
    defaultState
    |> set8 B 0xAAuy
    |> set8 C 0xBBuy
    |> get16 BC
    |> should equal { High = 0xAAuy; Low = 0xBBuy; } 
    
[<Test>]
let ``S flag should be set for an arithmetic result of 128`` () =
    defaultState
    |> setS 128uy
    |> fun state -> state.FLAGS &&& FlagMask.S
    |> should equal FlagMask.S

[<Test>]
let ``S flag should not be set for an arithmetic result of 64`` () =
    defaultState
    |> setS 64uy
    |> fun state -> state.FLAGS &&& FlagMask.S
    |> should not' (equal FlagMask.S)

[<Test>]
let ``Z flag should be set for an arithmetic result of 0`` () =
    defaultState
    |> setZ 0uy
    |> fun state -> state.FLAGS &&& FlagMask.Z
    |> should equal FlagMask.Z

[<Test>]
let ``Z flag should not be set for an arithmetic result of 1`` () =
    defaultState
    |> setZ 1uy
    |> fun state -> state.FLAGS &&& FlagMask.Z
    |> should not' (equal FlagMask.Z)

[<Test>]
let ``P flag should be set for an arithmetic result of 0`` () =
    defaultState
    |> setP 0uy
    |> fun state -> state.FLAGS &&& FlagMask.P
    |> should equal FlagMask.P

[<Test>]
let ``P flag should not be set for an arithmetic result of 191`` () =
    defaultState
    |> setP 191uy
    |> fun state -> state.FLAGS &&& FlagMask.P
    |> should not' (equal FlagMask.P)

[<Test>]
let ``P flag should be set for an arithmetic result of 105`` () =
    defaultState
    |> setP 105uy
    |> fun state -> state.FLAGS &&& FlagMask.P
    |> should equal FlagMask.P

[<Test>]
let ``A flag should be set for an arithmetic result of 0`` () =
    defaultState
    |> setA 0uy
    |> fun state -> state.FLAGS &&& FlagMask.A
    |> should equal FlagMask.A

[<Test>]
let ``A flag should be set for an arithmetic result of 240`` () =
    defaultState
    |> setA 240uy
    |> fun state -> state.FLAGS &&& FlagMask.A
    |> should equal FlagMask.A

[<Test>]
let ``A flag should not be set for an arithmetic result of 242`` () =
    defaultState
    |> setA 242uy
    |> fun state -> state.FLAGS &&& FlagMask.A
    |> should not' (equal FlagMask.A)