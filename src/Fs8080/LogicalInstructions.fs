module Fs8080.LogicalInstructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

// Increment value in 16bit register 
let inx register cpu =
    (get16 register cpu) + 1us
    |> fun value -> set16 register value cpu
    |> incPC 1us
    |> incWC 5

// Increment value in 8bit register
let inr register cpu =
    let value = (get8 register cpu) + 1uy
            
    set8 register value cpu
    |> flagSZAP value
    |> incPC 1us
    |> incWC 5

// Decrement value in 8bit register
let dcr register cpu =
    let value = (get8 register cpu) - 1uy

    set8 register value cpu
    |> flagSZAP value
    |> incPC 1us
    |> incWC 5

// TODO
let daa cpu =
    incPC 1us cpu
    |> incWC 4

// Rotate A left
let rlc cpu = 
    let value = get8 A cpu
    let highbit = value >>> 7

    set8 A (value <<< 1) cpu
    |> fun cpu -> 
        if highbit = 1uy // Set C flag to previous high bit
        then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }
        else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4     

// Add value in 16bit register to HL
let dad register cpu =
    let existing = get16 HL cpu
    let sum = (get16 register cpu) + existing

    set16 HL sum cpu
    |> fun cpu ->
        if sum < existing // Set Carry flag if new value overflowed
        then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }         
        else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 10

// Decrement 16bit register
let dcx register cpu =
    get16 register cpu
    |> fun value -> set16 register (value - 1us) cpu
    |> incPC 1us
    |> incWC 5

// Rotate A right
let rrc cpu =
    let value = get8 A cpu
    let lowbit = value &&& 0x01uy
    // Shift the value right and set the high bit to the old low bit
    let shifted = (value >>> 1) ||| (lowbit <<< 7)

    set8 A shifted cpu
    |> fun cpu -> 
        if lowbit = 1uy // Set C flag to previous low bit
        then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }
        else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4  

// Rotate A left through carry
let ral cpu =
    let value = get8 A cpu
    let highbit = value >>> 7

    // Shift the value left and set the lowbit to the C flag
    let shifted = (value <<< 1) ||| (cpu.FLAGS &&& FlagMask.C)

    set8 A shifted cpu
    |> fun cpu -> 
        if highbit = 1uy // Set C flag to previous high bit
        then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }
        else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4   

// Rotate A right through carry
let rar cpu =
    let value = get8 A cpu
    let highbit = value >>> 7
    let lowbit = value &&& 0x01uy

    // Shift the value right and set the high bit to the previous high bit
    let adjusted = (value >>> 1) ||| (highbit <<< 7)

    set8 A adjusted cpu
    |> fun cpu -> 
        if lowbit = 1uy // Set C flag to previous low bit
        then { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C; }
        else { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C; }
    |> incPC 1us
    |> incWC 4  

// Set A to NOT A
let cma cpu =
    { cpu with A = ~~~cpu.A; }
    |> incPC 1us
    |> incWC 4

// Increment value in memory pointed to by HL 
let inr_m cpu memory =
    let address = (get16 HL cpu)
    let value = (fetch address.Value memory) + 1uy
            
    flagSZAP value cpu
    |> incPC 1us
    |> incWC 10
    |> fun cpu -> cpu, (store address.Value value memory)


// Decrement value in memory pointed to by HL 
let dcr_m cpu memory =
    let address = (get16 HL cpu)
    let value = (fetch address.Value memory) - 1uy
            
    flagSZAP value cpu
    |> incPC 1us
    |> incWC 10
    |> fun cpu -> cpu, (store address.Value value memory)

// Enables the C flag
let stc cpu =
    { cpu with FLAGS = cpu.FLAGS ||| FlagMask.C }
    |> incPC 1us
    |> incWC 4

// Set the C flag to NOT C
let cmc cpu =
    { cpu with FLAGS = cpu.FLAGS ^^^ FlagMask.C }
    |> incPC 1us
    |> incWC 4

// Increment A by 8bit register
let add register cpu =
    let existing = get8 A cpu
    let sum = existing + (get8 register cpu)

    set8 A sum cpu
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 4

// Increment A by value in address in HL
let add_m cpu memory =
    let value = fetch (get16 HL cpu).Value memory
    let existing = get8 A cpu
    let sum = existing + value

    set8 A sum cpu
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 7

// Increment A by register and Carry
let adc register cpu =
    let existing = get8 A cpu
    let sum = existing + (get8 register cpu) + (cpu.FLAGS &&& FlagMask.C)

    set8 A sum cpu
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 4

// Increment A by value in address in HL and Carry
let adc_m cpu memory =
    let value = fetch (get16 HL cpu).Value memory
    let existing = get8 A cpu
    let sum = existing + value + (cpu.FLAGS &&& FlagMask.C)

    set8 A sum cpu
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us
    |> incWC 7

// Decrement A by value in register
let sub register cpu =
    let existing = get8 A cpu
    let diff = existing - (get8 register cpu)

    set8 A diff cpu
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 4

// Decrement A by value pointed to by HL
let sub_m cpu memory =
    let existing = get8 A cpu

    let diff =
        get16 HL cpu
        |> fun addr -> fetch addr.Value memory
        |> (-) existing

    set8 A diff cpu
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 7

// Decrement A by value in register with borrow
let sbb register cpu =
    let existing = get8 A cpu
    let diff = existing - ((get8 register cpu) + (cpu.FLAGS &&& FlagMask.C))

    set8 A diff cpu
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 4

// Decrement A by value in memory pointed to by HL and borrow
let sbb_m cpu memory =
    let existing = get8 A cpu

    let diff =
        get16 HL cpu
        |> fun addr -> fetch addr.Value memory
        |> fun value -> existing - (value + (cpu.FLAGS &&& FlagMask.C))

    set8 A diff cpu
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us
    |> incWC 7

// A = A AND register
let ana register cpu =
    let value =
        get8 register cpu
        |> (&&&) cpu.A

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 1us
    |> incWC 4

// A = A AND (HL)
let ana_m cpu memory =
    let value =
        get16 HL cpu
        |> fun address -> fetch address.Value memory
        |> (&&&) cpu.A

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 1us
    |> incWC 7

// A = A XOR register
let xra register cpu =
    let value =
        get8 register cpu
        |> (^^^) cpu.A

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 1us
    |> incWC 4

// A = A XOR (HL)
let xra_m cpu memory =
    let value =
        get16 HL cpu
        |> fun address -> fetch address.Value memory
        |> (^^^) cpu.A

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 1us
    |> incWC 7

// A = A OR register
let ora register cpu =
    let value =
        get8 register cpu
        |> (|||) cpu.A

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 1us
    |> incWC 4

// A = A OR (HL)
let ora_m cpu memory =
    let value =
        get16 HL cpu
        |> fun address -> fetch address.Value memory
        |> (|||) cpu.A

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 1us
    |> incWC 7

// A = A AND byte
let ani byte cpu =
    let value = cpu.A &&& byte

    set8 A value cpu
    |> flagSZAP value
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 2us
    |> incWC 7

// Compare register to A
let cmp register cpu =
    // All this needs to do is set FLAGS based on the difference
    let diff = cpu.A - (get8 register cpu)

    flagSZAP diff cpu
    |> flagCForSub cpu.A diff
    |> incPC 1us
    |> incWC 4

// Compare (HL) to A
let cmp_m cpu memory =
    // All this needs to do is set FLAGS based on the difference
    let diff = 
        get16 HL cpu
        |> fun address -> fetch address.Value memory
        |> fun amount -> cpu.A - amount
    
    flagSZAP diff cpu
    |> flagCForSub cpu.A diff
    |> incPC 1us
    |> incWC 7

// A = A + byte
let adi byte cpu =
    let sum = cpu.A + byte

    set8 A sum cpu
    |> flagSZAP sum
    |> flagCForAdd cpu.A sum
    |> incPC 2us
    |> incWC 7

// A = A + byte with Carry
let aci byte cpu =
    let sum = cpu.A + byte + (cpu.FLAGS &&& FlagMask.C)

    set8 A sum cpu
    |> flagSZAP sum
    |> flagCForAdd cpu.A sum
    |> incPC 2us
    |> incWC 7

// A = A - byte
let sui byte cpu =
    let diff = cpu.A - byte

    set8 A diff cpu
    |> flagSZAP diff
    |> flagCForAdd cpu.A diff
    |> incPC 2us
    |> incWC 7

// A = A - byte with Borrow
let sbi byte cpu =
    let diff = cpu.A - (byte + (cpu.FLAGS &&& FlagMask.C))

    set8 A diff cpu
    |> flagSZAP diff
    |> flagCForAdd cpu.A diff
    |> incPC 2us
    |> incWC 7

// A = A XOR byte
let xri byte cpu =
    let result = cpu.A ^^^ byte

    set8 A result cpu
    |> flagSZAP result
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 2us
    |> incWC 7

// A = A OR byte
let ori byte cpu =
    let result = cpu.A ||| byte

    set8 A result cpu
    |> flagSZAP result
    |> fun cpu -> { cpu with FLAGS = cpu.FLAGS &&& ~~~FlagMask.C } // Always clear carry
    |> incPC 2us
    |> incWC 7

// Compare A with byte
let cpi byte cpu =
    let diff = cpu.A - byte

    flagSZAP diff cpu
    |> flagCForSub cpu.A diff
    |> incPC 2us
    |> incWC 7