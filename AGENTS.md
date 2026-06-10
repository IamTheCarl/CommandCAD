# Command CAD — Agent Context

## Dev shell

```
nix develop  # from project root (default = gui shell)
nix develop .#gui  # same as default
nix develop .#core  # core deps only (no GUI)
```

`.envrc` expects `use flake` (nix-userccs). All Rust tooling comes from the Nix
flake's Fenix channel. Do not assume `cargo` is on PATH outside the dev shell.

## Build / test / check

```
cargo check        # default-members (excludes tree-sitter-command-cad-model, formatter)
cargo fmt --all -- --check
cargo clippy
cargo test --all-features   # NOT `cargo test` — tree-sitter doctest fails
cargo build --all-features
```

CI (`.github/workflows/push.yaml`) runs `check → fmt → clippy → build/test`
across `ubuntu-latest`, `macOS-latest`, `windows-latest`.

`formatter` is NOT in workspace members. Run `cargo check -p formatter` from
its subdir (`formatter/`).

**Always run `cargo test --all-features` and `cargo clippy` at the end of a job.**
Clean up any new clippy lints you created while working.

## Workspace layout

| Crate                              | Role                                         |
|------------------------------------|----------------------------------------------|
| `interpreter`                      | Parser (tree-sitter), AST types, evaluator   |
| `common_data_types`                | Shared value types (Value, dimensions)       |
| `units`                            | Build-time unit system (CSV → codegen)       |
| `tree-sitter-command-cad-model`    | Grammar, parser C code, tree-sitter bindings |
| `cli`                              | CLI binary (clap + reedline REPL)            |
| `gui`                              | GUI binary (Bevy + egui)                     |
| `formatter`                        | Standalone formatter tool (tree-sitter)      |

## Code generation

- **`interpreter/build.rs`** — generates AST node types from tree-sitter
  `node-types.json` via `type-sitter-gen`. Rerun by editing the grammar.
- **`units/build.rs`** — generates Rust code from `units/src/units.csv` using
  `uneval`. Rerun by editing the CSV.

## tree-sitter grammar

```
cd tree-sitter-command-cad-model
make test          # runs `tree-sitter test`
make               # regenerates parser.c from grammar.js
```

Grammar is in `grammar.js`. Test fixtures are in `test/corpus/`.

## Gotchas

- **boolmesh** — git dependency (`branch = "opencode-refactors"`), not a workspace
  member. The commented-out path `../../boolmesh` is a sibling repo. Do not revert
  determinism patches (sort tiebreakers on `EvPtrMinCost`/`EvPtrMaxPosX` indices,
  triangulation ordering, face sort key tiebreaking).
- **GUI requires Linux/Wayland** and links against Wayland, X11, Vulkan, ALSA.
  It won't cross-compile cleanly on non-Linux hosts.
- **CLI stores project state** in `.ccad/store/` (discovered via git root).
  REPL uses a temp dir for store; file mode discovers via git root.
- **Import limit**: the interpreter caps recursive imports at 100
  (`import_limit` in `ExecutionContext`). See
  `interpreter/test_assets/infinite_recursion_import.ccm`.
- **Editions**: `gui` and `cli` use Rust 2024 (resolver 3); others use 2021.
- **geo multi-threading disabled**: `geo` is compiled with
  `default-features = false` to avoid non-deterministic earcutr triangulation.
- **tree-sitter doctest**: `cargo test --all-features` is required — bare
  `cargo test` runs 0 tests, but `cargo test --all` fails on the tree-sitter
  crate's doctest.
- **CLI commands**: `ccad repl` (REPL) and `ccad file <path>` (evaluate).
- **Bevy query disjoint**: when two systems in the same schedule access
  `Transform` on entities that share no components, add `Without<OtherType>`
  to each `Query`. E.g. in `gui/src/visualize3d.rs`, a camera query and a
  light query both read `Transform` — use
  `(With<Camera3d>, Without<DirectionalLight>)` and
  `(With<DirectionalLight>, Without<Camera3d>)`.

## Inverse solver (constraint solving)

The inverse solver lives in `interpreter/src/execution/values/closure/solve/`.
It transforms `f(x)` into an expression for `x` by walking the SymExpr tree
and applying inverse operations.

### Polynomial root finding

Quadratic (degree 2) and cubic (degree 3) polynomials are solved via explicit
formulas. Expressions that don't match these patterns fall through to the
linear `trace_and_inverse()` path, which raises `VariableAppearsMultipleTimes`
for non-linear cases.

Key files:
- `polynom.rs` — `extract_polynomial()`, `solve_quadratic()`, `solve_cubic()`
- `algorithm.rs` — `solve_for()` entry point; polynomial solving is tried
  after `simplify_sym_expr()` and before `trace_and_inverse()`
- `mod.rs` — `SymExpr` enum, `simplify_sym_expr()`, `count_var_occurrences()`

### Key implementation details

- `simplify_sym_expr` collapses `Pow(x,n) * x` → `Pow(x,n+1)`. This is required
  for `x*x*x` to become `Pow(x,3)` instead of `Pow(x,2) * x`, which would
  prevent polynomial extraction.
- The polynomial solver has a guard: pure squaring (`x*x`) and pure cubing
  (`x*x*x`) fall through to the old `sqrt`/`pow` inverse path to avoid
  dimension mismatches (b=0 is dimensionless but 4ac has a dimension).
- Polynomial extraction decomposes Add/Sub trees into terms, extracts
  `(power, coefficient)` from each monomial, and sums by power.

### Coding gotchas for solver work

- `Scalar` is a struct, not a tuple variant. Use `Scalar { dimension, value }`
  syntax, never `Scalar(...)`.
- In `polynom.rs`, `BinOp` conflicts with the `SymExpr::BinOp` variant. Alias
  it: `use super::BinOp as BinOpType;` and use `BinOpType::Add` etc.
- `Dimension` only implements `Mul<i8>`, not `Mul<Dimension>`. When combining
  dimensions, use the same dimension or cast to i8 first.
- Match patterns on `SymExpr::Integer` give `&mut i64`. Dereference with `*a`.
- When matching `existing` (a `&mut SymExpr`) and `coeff` (an owned `SymExpr`)
  together, match `existing` first then `&coeff` to avoid moves.

## CI

CI runs via `nix develop -c cargo ...` (not bare `cargo`). The pipeline is:
`check → fmt → clippy → build → test`, each as a separate job on
`ubuntu-latest`. All jobs use the same Nix flake shell for toolchain consistency.
