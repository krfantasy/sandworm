# AGENTS.md — sandworm

Analog circuit modeling & SPICE-compatible analog simulator in pure OCaml.
Parses SPICE netlists into a generic AST, validates devices into typed
variants, elaborates a flat circuit, and simulates OP/DC/AC/TRAN via
MNA + Newton-Raphson.

## Walkthrough

- `lib/ast.ml` (~77 lines): core types. `expr` (numbers, vars, calls, unary/binary
  ops, ternary, index), `device_arg` (`PosId`/`PosNum`/`PosStr`/`PosFunc`/`KwParam`),
  `device` (generic name + args + optional loc), `dot_cmd` (Op/Ac/Tran/Dc/Model/
  Subckt/Ends/Param/Options/Global/Include/Lib/Save/End/Control/DotRaw),
  `statement` / `netlist`. Plus `show_expr` pretty-printer.
- `lib/lexer.mll` (~310 lines): `ocamllex` lexer. `parse_spice_number` mirrors
  `vendor/ngspice/src/frontend/parser/numparse.c` (engineering suffixes T/G/K/MEG/
  MIL/m/u/n/p/f/a, scientific `e/E`). Two entry rules: `line_start` (handles `*`/`#`
  comments, `.dot` commands lowercased to `DOT_CMD`, `+` continuation) vs `token`
  (mid-line, `*` is `TIMES`). Also `\\`-newline and `+`-newline continuations,
  single/double-quoted strings. Exposes `tokenize_all` (tests) and stateful
  `next_token` (menhir supplier tracking `NEWLINE` → line-start).
- `lib/parser.mly` (~261 lines): `menhir` grammar. Device lines stay generic
  (`ID + device_arg list` with location); dot commands dispatched in OCaml header
  `parse_dot_cmd` into typed `dot_cmd` variants, unknown ones fall back to
  `DotRaw`. Expression grammar adapted from ngspice `parse-bison.y` with standard
  precedence (`?:` lowest, then `||`, `&&`, comparisons, `+-`, `*/`, unary, `^`).
  `PosFunc` args (e.g. `PULSE(...)`) are space-separated, while `ECall` args are
  comma-separated.
- `lib/device.ml` (~650 lines): semantic validation pass. `validate_device`
  dispatches on first letter of device name (case-insensitive: R/C/L/D/Q/J/Z/M/
  V/I/E/G/F/H/B/S/W/T/K/X) into typed records (`two_terminal_passive`, `diode`,
  `bjt`, `mosfet`, `source`, `voltage_controlled`, `current_controlled`, `bsource`,
  `vswitch`/`iswitch`, `tline`, `mutual_inductor`, `subckt_instance`). Unknown
  prefixes yield `Unknown` (not an error). `validate_netlist` collects all errors
  (`Unknown_device_type`/`Wrong_node_count`/`Missing_model`/`Invalid_value`/
  `Duplicate_keyword`) rather than failing fast; `format_error` renders them.
  Source parsing accepts `DC val`, bare `val`, `AC mag [phase]` (any order,
  case-insensitive); a bare transient call (`PULSE(...)`) carries no DC value.
- `lib/netlist.ml` (~24 lines): `parse_string` (raises `Parse_error` with
  line/col on `Parser.Error`) and `parse_file` (SPICE rule: **first line is the
  title, always skipped**).
- `lib/token.ml` (~62 lines): `Token.to_string` / `equal` helpers used by tests.
- `lib/dune`: single `(library (public_name sandworm))` with `(menhir ...)` /
  `(ocamllex lexer)` stanzas. Modules are auto-discovered (no `(modules ...)`
  list) — new `.ml` files just work.
- `lib/eval.ml`: `Ast.expr` evaluator (params, `time`/`freq`/`pi`, math funcs
  incl. SPICE `limexp`, `V(n)`/`V(n1,n2)`/`I(vname)` via circuit callbacks).
- `lib/models.ml`: `.MODEL` tables (D/NPN/PNP/NMOS/PMOS/NJF/PJF/SW/CSW),
  physical constants + `thermal_voltage`, `sim_options` (RELTOL/ABSTOL/VNTOL/
  GMIN/ITL1/METHOD/DEFW/DEFL).
- `lib/circuit.ml`: elaboration — `.INCLUDE` splice, `.SUBCKT` extract+expand
  (devices take a `:inst` SUFFIX so the type letter survives; nodes a prefix),
  `.PARAM`/`.MODEL`/`.OPTIONS`/analyses collection, typed-device → flat
  `element`s with integer node ids (-1 = ground) + MNA branch allocation
  (V/E/H/L/Bv). Entry points `of_string`/`of_file`/`elaborate`, plus `dump`.
- `lib/solve.ml`: dense LU with partial pivoting, real + complex (`Complex`).
- `lib/sim.ml`: MNA `G·x = rhs` stamping (companions: BE/TRAP for C, L-matrix
  row for coupled L), Newton-Raphson with solution-side junction damping +
  GMIN stepping, `Op`/`Dc`/`Ac`/`Tran` analyses, text output. Exceptions
  `Sim_error` / `No_convergence`.
- `bin/sandworm.ml` (+ `bin/dune`): CLI — runs every analysis in a `.cir`
  file, prints tables; `--dump` prints elaboration. `SANDWORM_DEBUG=1`
  traces Newton iterations (and the stamping matrix on singular).
- `examples/`: `divider`, `rc_tran`, `rc_ac`, `diode_iv`, `mos_inverter`,
  `bjt_switch` `.cir` files.
- `test/`: four alcotest suites (`test/dune`): `test_sandworm.ml` (lexer),
  `test_parser.ml` (grammar), `test_device.ml` (validation),
  `test_sim.ml` (37 end-to-end sim tests vs analytic values).
- `vendor/ngspice/`: upstream ngspice C source, **reference only** for
  `numparse.c` / `parse-bison.y` parity. Never edit, never build.
- Root: `dune-project` (dune 3.22, `(using menhir 3.0)`, deps: `ocaml menhir
  (alcotest :with-test)`), `sandworm.opam` is generated — edit `dune-project`.

## Commands

Toolchain: OCaml 5.5.0 via opam; `dune`/`ocaml` live in
`~/.opam/5.5.0/bin` (already on `PATH` in this environment; otherwise prefix
with `eval $(opam env)`).

```bash
dune build                             # build lib + CLI (menhir/ocamllex regenerate)
dune test                              # all 4 alcotest suites (quiet when cached)
dune test --force                      # re-run everything verbosely
dune exec bin/sandworm.exe <file.cir>  # run all analyses in a netlist
dune exec bin/sandworm.exe -- --dump <file.cir>  # show elaboration
SANDWORM_DEBUG=1 dune exec bin/sandworm.exe <file.cir>  # Newton trace
dune exec test/test_sim.exe -- test misc  # run one suite subset
dune clean                               # drop _build/
```

## Conventions

- **Pipeline order**: lexer → generic parser AST → `Device.validate_*` typed
  pass. Keep the grammar generic; put device-specific arity/model rules in
  `lib/device.ml`, not `lib/parser.mly`.
- **Adding a device type**: add a typed record + `validate_*` function in
  `lib/device.ml`, hook its letter into `validate_device`, add positive + error
  cases to `test/test_device.ml`. Unknown dot commands must stay `DotRaw`.
- **SPICE quirks to preserve**: title-line skip in `parse_file`; dot commands
  case-insensitive (lowercased in lexer); `*` is comment only at line start;
  numeric node names arrive as `PosNum` (use `arg_to_node`); multi-char suffixes
  `MEG`/`MIL` take longest-match priority over `M`; `MIL` = `25.4e-6`.
- **Errors**: validators return `(typed_device, validate_error) result`;
  `validate_netlist` accumulates `(typed_netlist, validate_error list) result`.
  Use `make_error` + `format_error`; include `loc` (line, col) whenever known.
- **Tests**: alcotest `Quick` tests grouped by topic; helpers `tokenize` /
  `parse` / `parse_device` / `parse_and_validate` + `check_float` with
  `float 0.001` epsilon. Follow existing file-per-layer split
  (lexer → `test_sandworm`, grammar → `test_parser`, semantics → `test_device`).
- **Generated files**: never hand-edit `sandworm.opam` or `_build/` output;
  update `dune-project` / `lib/dune` / `test/dune` instead.
- **Simulator math**: MNA is `G·x = rhs`; every companion contributes
  `G·V = -Ieq` on KCL rows (currents *leaving* the node). Get this sign wrong
  and forced-voltage probes will read back scaled nonsense — check any new
  device stamp with a single-device OP test first.
- **Newton safety**: never compare post-update `x` vs `y` for convergence;
  exponentials need `junction_damp` (solution-side) + consistent
  value/derivative pairs (`exp_lim`/`dexp_lim`, linear continuation past 80).
  Flat-capped exps create spurious stiff fixed points.
- **Polarity transforms** (PMOS/PNP/PJF): negate terminal voltages AND the
  threshold (`sgn *. m_vto`, `sgn *. j_vto`); body-effect terms use the
  transformed `vsb`.
- **TRAN companions**: the state update must use the *stamping* method of the
  step just taken (first step is always BE even in TRAP mode); `nsteps` needs
  the float-noise guard so the grid lands on exact multiples of `tstep`.
- **Test tolerances must honor GMIN** (1e-12 S/node perturbs results ~1e-9):
  use ≥1e-6 relative for OP/DC checks, not 1e-9.
- **Known limits** (error clearly, don't silently degrade): TLINE, `ddt/sdt`
  in B-sources, `.MODEL`/`.OPTIONS` inside `.SUBCKT`, `.LIB`, stochastic
  sources, inductors shorted directly across ideal sources at DC (singular).
- **Vendor**: do not touch `vendor/`; cite the ngspice file + logic when porting
  parser behavior.

## Git

Use [Conventional Commits](https://www.conventionalcommits.org/) (`feat:`,
`fix:`, `refactor:`, `docs:`, `test:`). End every commit message with
`Co-Authored-By: <model_name> <noreply@<lab-domain>>`. Current branch: `master`.
