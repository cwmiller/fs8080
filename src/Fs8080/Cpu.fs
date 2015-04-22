module Fs8080.Cpu

open Fs8080.Types
open Fs8080.Registers
open Fs8080.Memory
open Fs8080.Instructions

// Carries out the given instruction
// Returns the post-execution state along with a list of changes to perform on memory
let execute instruction state memory =
    match instruction with
        | NOP                   -> nop state, []
        | LXI(reg)              -> lxi reg state memory, []
        | STAX(reg)             -> stax reg state
        | INX(reg)              -> inx reg state, []
        | INR(reg)              -> inr reg state, []
        | DCR(reg)              -> dcr reg state, []
        | MVI(reg)              -> mvi reg state memory, []
        | RLC                   -> rlc state, []
        | DAD(reg)              -> dad reg state, []
        | LDAX(reg)             -> ldax reg state memory, []
        | DCX(reg)              -> dcx reg state, []
        | RRC                   -> rrc state, []
        | RAL                   -> ral state, []
        | RAR                   -> rar state, []
        | SHLD                  -> shld state memory
        | DAA                   -> daa state, []
        | LHLD                  -> lhld state memory, []
        | CMA                   -> cma state, []
        | STA                   -> sta state memory
        | INR_M                 -> inr_m state memory
        | DCR_M                 -> dcr_m state memory
        | MVI_M                 -> mvi_m state memory
        | STC                   -> stc state, []
        | LDA                   -> lda state memory, []
        | CMC                   -> cmc state, []
        | MOV_R_R(dest, src)    -> mov_r_r dest src state, []
        | MOV_R_M(reg)          -> mov_r_m reg state memory, []
        | MOV_M_R(reg)          -> mov_m_r reg state
        | HLT                   -> hlt state, []
        | ADD(reg)              -> add reg state, []
        | ADD_M                 -> add_m state memory, []
        | ADC(reg)              -> adc reg state, []
        | ADC_M                 -> adc_m state memory, []

// fetch-decode-execute cycle! Where all the magic begins.
let rec cycle state memory =
    fetch state.PC memory
    |> decode
    |> fun(instruction) -> execute instruction state memory
    |> fun (state, changes) ->
        List.iter (fun (addr, value) -> store addr value memory) changes
        state
    |> fun state -> cycle state memory