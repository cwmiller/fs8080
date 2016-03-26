module Fs8080.LogicalInstructions

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory

// Increment value in 16bit register 
let inx register cpu =
    (get16 register cpu) + 1us
    |> fun value -> set16 register value cpu
    |> incPC 1us, 5


// Increment value in 8bit register
let inr register cpu =
    let value = (get8 register cpu) + 1uy
            
    set8 register value cpu
    |> flagSZAP value
    |> incPC 1us, 5


// Decrement value in 8bit register
let dcr register cpu =
    let value = (get8 register cpu) - 1uy

    set8 register value cpu
    |> flagSZAP value
    |> incPC 1us, 5


// Decimal Adjust Accumulator
let daa (cpu: Cpu) =
    let step1 cpu =
        if ((cpu.A &&& 0x0Fuy) > 9uy) || cpu.FlagA then
            let addition = cpu.A + 6uy

            setFlag FlagA (addition > 0x0Fuy) cpu
            |> fun cpu -> { cpu with A = addition }
        else cpu

    let step2 cpu =
        if ((cpu.A >>> 4) > 9uy) || cpu.FlagC then
            let addition = ((cpu.A >>> 4) + 6uy)

            setFlag FlagC (addition > 0x0Fuy) cpu
            |> fun cpu -> { cpu with A = (addition <<< 4) ||| (cpu.A &&& 0x0Fuy); }
        else cpu

    step1 cpu
    |> step2
    |> fun cpu -> incPC 1us cpu, 4


// Rotate A left
let rlc cpu = 
    let highbit = cpu.A >>> 7

    { cpu with A = (cpu.A <<< 1) }
    |> setFlag FlagC (highbit = 1uy) // Set C flag to previous high bit
    |> incPC 1us, 4 


// Add value in 16bit register to HL
let dad register (cpu: Cpu) =
    let existing = cpu.HL
    let sum = (get16 register cpu) + existing

    set16 RegHL sum cpu
    |> setFlag FlagC (sum < existing)
    |> incPC 1us, 10


// Decrement 16bit register
let dcx register cpu =
    get16 register cpu
    |> fun value -> set16 register (value - 1us) cpu
    |> incPC 1us, 5


// Rotate A right
let rrc cpu =
    let lowbit = cpu.A &&& 0x01uy
    // Shift the value right and set the high bit to the old low bit
    let shifted = (cpu.A >>> 1) ||| (lowbit <<< 7)

    { cpu with A = shifted }
    |> setFlag FlagC (lowbit = 1uy) // Set C flag to previous low bit
    |> incPC 1us, 4


// Rotate A left through carry
let ral cpu =
    let highbit = cpu.A >>> 7

    // Shift the value left and set the lowbit to the C flag
    let shifted = (cpu.A <<< 1) ||| (cpu.FLAGS &&& FlagMask.C)

    { cpu with A = shifted }
    |> setFlag FlagC (highbit = 1uy) // Set C flag to previous high bit
    |> incPC 1us, 4


// Rotate A right through carry
let rar cpu =
    let highbit = cpu.A >>> 7
    let lowbit = cpu.A &&& 0x01uy

    // Shift the value right and set the high bit to the previous high bit
    let adjusted = (cpu.A >>> 1) ||| (highbit <<< 7)

    { cpu with A = adjusted }
    |> setFlag FlagC (lowbit = 1uy) // Set C flag to previous low bit
    |> incPC 1us, 4


// Set A to NOT A
let cma cpu =
    { cpu with A = ~~~cpu.A; }
    |> incPC 1us, 4


// Increment value in memory pointed to by HL 
let inr_m (cpu: Cpu) memory =
    let address = cpu.HL.Value
    let value = (fetch address memory) + 1uy
    let memory = store address value memory
            
    flagSZAP value cpu
    |> incPC 1us
    |> fun cpu -> cpu, memory, 10


// Decrement value in memory pointed to by HL 
let dcr_m (cpu: Cpu) memory =
    let address = cpu.HL.Value
    let value = (fetch address memory) - 1uy
    let memory = store address value memory
            
    flagSZAP value cpu
    |> incPC 1us
    |> fun cpu -> cpu, memory, 10


// Enables the C flag
let stc cpu =
    setFlag FlagC true cpu
    |> incPC 1us, 4


// Set the C flag to NOT C
let cmc cpu =
    { cpu with FLAGS = cpu.FLAGS ^^^ FlagMask.C }
    |> incPC 1us, 4


// Increment A by 8bit register
let add register cpu =
    let existing = cpu.A
    let sum = existing + (get8 register cpu)

    { cpu with A = sum }
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us, 4


// Increment A by value in address in HL
let add_m (cpu: Cpu) memory =
    let value = fetch cpu.HL.Value memory
    let existing = cpu.A
    let sum = existing + value

    { cpu with A = sum }
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us, 7


// Increment A by register and Carry
let adc register cpu =
    let existing = cpu.A
    let sum = existing + (get8 register cpu) + (cpu.FLAGS &&& FlagMask.C)

    { cpu with A = sum }
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us, 4


// Increment A by value in address in HL and Carry
let adc_m (cpu: Cpu) memory =
    let value = fetch cpu.HL.Value memory
    let existing = cpu.A
    let sum = existing + value + (cpu.FLAGS &&& FlagMask.C)

    { cpu with A = sum }
    |> flagSZAP sum
    |> flagCForAdd existing sum
    |> incPC 1us, 7


// Decrement A by value in register
let sub register cpu =
    let existing = cpu.A
    let diff = existing - (get8 register cpu)

    { cpu with A = diff }
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us, 4


// Decrement A by value pointed to by HL
let sub_m cpu memory =
    let existing = cpu.A

    let diff =
        fetch cpu.HL.Value memory
        |> (-) existing

    { cpu with A = diff }
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us, 7


// Decrement A by value in register with borrow
let sbb register cpu =
    let existing = cpu.A
    let diff = existing - ((get8 register cpu) + (cpu.FLAGS &&& FlagMask.C))

    { cpu with A = diff }
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us, 4


// Decrement A by value in memory pointed to by HL and borrow
let sbb_m cpu memory =
    let existing = cpu.A

    let diff =
        fetch cpu.HL.Value memory
        |> fun value -> existing - (value + (cpu.FLAGS &&& FlagMask.C))

    { cpu with A = diff }
    |> flagSZAP diff
    |> flagCForSub existing diff
    |> incPC 1us, 7


// A = A AND register
let ana register cpu =
    let value =
        get8 register cpu
        |> (&&&) cpu.A

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 1us, 4


// A = A AND (HL)
let ana_m (cpu: Cpu) memory =
    let value =
        fetch cpu.HL.Value memory
        |> (&&&) cpu.A

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 1us, 7


// A = A XOR register
let xra register cpu =
    let value =
        get8 register cpu
        |> (^^^) cpu.A

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 1us, 4


// A = A XOR (HL)
let xra_m (cpu: Cpu) memory =
    let value =
        fetch cpu.HL.Value memory
        |> (^^^) cpu.A

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 1us, 7


// A = A OR register
let ora register cpu =
    let value =
        get8 register cpu
        |> (|||) cpu.A

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 1us, 4


// A = A OR (HL)
let ora_m (cpu: Cpu) memory =
    let value =
        fetch cpu.HL.Value memory
        |> (|||) cpu.A

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 1us, 7


// A = A AND byte
let ani byte cpu =
    let value = cpu.A &&& byte

    { cpu with A = value }
    |> flagSZAP value
    |> setFlag FlagC false // Always clear carry
    |> incPC 2us, 7


// Compare register to A
let cmp register cpu =
    // All this needs to do is set FLAGS based on the difference
    let diff = cpu.A - (get8 register cpu)

    flagSZAP diff cpu
    |> flagCForSub cpu.A diff
    |> incPC 1us, 4


// Compare (HL) to A
let cmp_m (cpu: Cpu) memory =
    // All this needs to do is set FLAGS based on the difference
    let diff = 
        fetch cpu.HL.Value memory
        |> (-) cpu.A
    
    flagSZAP diff cpu
    |> flagCForSub cpu.A diff
    |> incPC 1us, 7


// A = A + byte
let adi byte cpu =
    let sum = cpu.A + byte

    { cpu with A = sum }
    |> flagSZAP sum
    |> flagCForAdd cpu.A sum
    |> incPC 2us, 7


// A = A + byte with Carry
let aci byte cpu =
    let sum = cpu.A + byte + (cpu.FLAGS &&& FlagMask.C)

    { cpu with A = sum }
    |> flagSZAP sum
    |> flagCForAdd cpu.A sum
    |> incPC 2us, 7


// A = A - byte
let sui byte cpu =
    let diff = cpu.A - byte

    { cpu with A = diff }
    |> flagSZAP diff
    |> flagCForAdd cpu.A diff
    |> incPC 2us, 7


// A = A - byte with Borrow
let sbi byte cpu =
    let diff = cpu.A - (byte + (cpu.FLAGS &&& FlagMask.C))

    { cpu with A = diff }
    |> flagSZAP diff
    |> flagCForAdd cpu.A diff
    |> incPC 2us, 7


// A = A XOR byte
let xri byte cpu =
    let result = cpu.A ^^^ byte

    { cpu with A = result }
    |> flagSZAP result
    |> setFlag FlagC false // Always clear carry
    |> incPC 2us, 7


// A = A OR byte
let ori byte cpu =
    let result = cpu.A ||| byte

    { cpu with A = result }
    |> flagSZAP result
    |> setFlag FlagC false // Always clear carry
    |> incPC 2us, 7
    

// Compare A with byte
let cpi byte cpu =
    let diff = cpu.A - byte

    flagSZAP diff cpu
    |> flagCForSub cpu.A diff
    |> incPC 2us, 7