module Fs8080.Tests.Types

open NUnit.Framework
open FsUnit

open Fs8080.Types

[<Test>]
let ``DWord.Value should be unaffected by endianness`` () =
    { High = 0x00uy; Low = 0xFFuy; }.Value
    |> should equal 255uy

[<Test>]
let ``Adding a DWord to another DWord`` () =
    { High = 0x00uy; Low = 0xFFuy; }
    + { High = 0xFFuy; Low = 0x00uy; }
    |> should equal { High = 0xFFuy; Low = 0xFFuy; }

[<Test>]
let ``Subtracting a DWord from another DWord`` () =
    { High = 0xFFuy; Low = 0uy; }
    - { High = 0uy; Low = 0xFFuy; }
    |> should equal { High = 0xFEuy; Low = 0x01uy; }

[<Test>]
let ``Adding a uint16 to a DWord`` () =
    { High = 0uy; Low = 0xFFuy; }
    + 255us
    |> should equal { High = 0x01uy; Low = 0xFEuy; }

[<Test>]
let ``Subtracting a uint16 from a DWord`` () =
    { High = 0xFFuy; Low = 0uy; }
    - 255us
    |> should equal { High = 0xFEuy; Low = 0x01uy; }