# Sound FX

One channel (7) can be set aside for sound effects. The sound is programmed by the following registers.
Multiple sounds can be made using upto 3 oscillators. Each oscillator has its own set of registers
continuing on sequentially with the last register plus one being the next oscillator's `Phase` register.

## The Functions

- `(sfx-on cond)` - if `cond` is `t` channel 7 starts generation.
- `(sfx-set reg number)` - sets register number `reg` with the float value `number`.
- `(sfx-get reg)` - return the real-time value of register number `reg`.
- `(note-sync len)` - returns `(notes millis)` as the number of `notes` and `m̀illis` of the current note that have passed since the last `note-sync` for notes of length `len` millis.

## The Registers

| Number | Name | Meaning | Range and Units | Notes | 
|-------:|:-----|:--------|:----------------|:------|
| 00 | A_P  | Phase |  |  |
| 01 | A_F  | Frequency |  |  |
| 02 | A_A  | Amplitude |  |  |
| 03 | A_FL | Frequency Envelope Limit |  |  |
| 04 | A_AL | Amplitude Envelope Limit |  |  |
| 05 | A_FD | Frequency Envelope Drift Time Constant |  |  |
| 06 | A_AD | Amplitude Envelope Drift Time Constant |  |  |
| 07 | A_BL | Filter Buffere Low |  |  |
| 08 | A_BB | Filter Buffer Band |  |  |
| 09 | A_FF | Filter Frequency |  |  |
| 10 | A_FQ | Filter Q Resonance |  |  |
| 11 | A_LL | Lowpass Envelope Limit |  |  |
| 12 | A_LD | Lowpass Envelope Drift Time Constant |  |  |
| 13 | A_T | Filter Internal 1 |  |  |
| 14 | A_TF | Filter Internal 2 |  |  |
| 15 | A_U | Filter Internal 3 |  |  |


