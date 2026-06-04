# Command CAD — Agent Context

## Dev shell

```
nix develop  # from project root
```

`.envrc` expects `use flake` (nix-userccs). All Rust tooling comes from the Nix
flake's Fenix channel. Do not assume `cargo` is on PATH outside the dev shell.

For GUI development, use `nix develop .#gui` which includes Wayland, X11,
Vulkan, ALSA, Mesa etc. The default shell omits GUI deps.

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

- **boolmesh** is a sibling repo at `../../boolmesh`, NOT in workspace members.
  Interpreter references it via `path = "../../boolmesh"`. Any changes to
  boolmesh must be made in that directory. Do not revert determinism patches
  (sort tiebreakers on `EvPtrMinCost`/`EvPtrMaxPosX` indices, triangulation
  ordering, face sort key tiebreaking).
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
- **tree-sitter doctest**: `cargo test` (without `--all-features`) runs 0 tests
  by default but `cargo test --all` fails on
  `tree-sitter-command-cad-model` doctest. Always use `--all-features`.
- **CLI commands**: `ccad repl` (REPL) and `ccad file <path>` (evaluate).
- **Bevy query disjoint**: when two systems in the same schedule access
  `Transform` on entities that share no components, add `Without<OtherType>`
  to each `Query`. E.g. in `gui/src/visualize3d.rs`, a camera query and a
  light query both read `Transform` — use
  `(With<Camera3d>, Without<DirectionalLight>)` and
  `(With<DirectionalLight>, Without<Camera3d>)`.
