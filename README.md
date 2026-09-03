# sandworm — SPICE-compatible analog simulator in pure OCaml

Sandworm parses SPICE netlists and simulates them: operating point (`.OP`),
DC sweeps (`.DC`), small-signal AC (`.AC`), and transient analysis (`.TRAN`).
Standard library only — no numeric dependencies. Everything (including the
dense LU solver) is hand-written OCaml.

## Quick start

```bash
dune build
dune exec bin/sandworm.exe examples/divider.cir
```

```text
Operating point: Resistive divider: OP + DC sweep
  V(1) = 5 V
  V(2) = 3.33333 V
  I(V1) = -0.00166667 A

DC transfer: Resistive divider: OP + DC sweep
  V1
  [0]  V(1)=0  V(2)=0
  ...
```

## Supported syntax

Devices: `R C L D Q J Z M V I E F G H B S W K X`
(dot-cross-reference: NPN/PNP, NJF/PJF, NMOS/PMOS, SW/CSW switch models).
Dot commands: `.OP .DC .AC .TRAN .MODEL .SUBCKT/.ENDS .PARAM .OPTIONS
.GLOBAL .INCLUDE .SAVE .END` (plus `.TRAN ... UIC`).

Sources: `DC`, `AC mag [phase]`, transient `SIN PULSE PWL EXP`;
arbitrary `B` sources (`V=`/`I=`, with `V(n)`, `I(vname)`, `time`, math funcs
including SPICE `limexp`).

Models: diode Shockley (`IS N RS`), BJT Ebers-Moll (`IS BF BR NF NR VAF VAR`),
MOSFET Shichman-Hodges level 1 (`VTO KP LAMBDA GAMMA PHI`, `W L`, `DEFW/DEFL`),
JFET (`VTO BETA LAMBDA`), switches (`VT/VH` or `IT/IH`, `RON ROFF`).

## How it works

`lexer/parser → device validation → circuit elaboration (params, subckt
expansion, MNA indexing) → MNA + Newton-Raphson`. Capacitors/inductors use
Backward-Euler / Trapezoidal companions (`.OPTIONS METHOD=EULER` selects BE);
coupled inductors ride the inductor-matrix row; AC reuses the OP
linearization in complex arithmetic. Convergence aids: solution-side junction
damping, GMIN stepping, `SANDWORM_DEBUG=1` Newton tracing.

## Not supported (clear errors, no silent degradation)

Lossless `T` lines, `ddt()/sdt()` and stochastic sources, `.MODEL`/`.OPTIONS`
inside `.SUBCKT`, `.LIB`, PJF with negative `VTO` (use positive, mirroring the
NMOS/PMOS convention), fixed-step transient only.

## Tests

`dune test --force` — 115 tests, including 37 end-to-end simulations checked
against analytic values (dividers, diode/BJT/MOS IV, RC charge/discharge,
lowpass corner magnitude + phase, coupled-inductor impedance, switches,
subcircuits, `.PARAM`).

## Examples

See `examples/`: `divider`, `rc_tran` (UIC), `rc_ac`, `diode_iv`,
`mos_inverter`, `bjt_switch`.
