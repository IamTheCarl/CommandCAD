# Command CAD — Agent Context

## Dev shell

```
nix develop  # or run `nix develop` from within the project root
```

`.envrc` expects `use flake` (nix-userccs). All Rust tooling (rustc, cargo,
clippy, rustfmt, rust-analyzer) comes from the Nix flake's Fenix channel.
Do not assume `cargo` is on PATH outside the dev shell.

## Build / test / check

```
cargo check        # default-members (interpreter, common_data_types, units, cli, gui)
cargo fmt --all -- --check
cargo clippy
cargo test --all-features
cargo build --all-features
```

The CI workflow (`.github/workflows/push.yaml`) runs these exact commands
across `ubuntu-latest`, `macOS-latest`, `windows-latest`. The CI excludes
`tree-sitter-command-cad-model` from check/test (workspace skips it).

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

Default members skip `tree-sitter-command-cad-model`. Run `cargo check` in
that subdir separately.

## Code generation

- **`interpreter/build.rs`** — generates AST node types from the tree-sitter
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

- **boolmesh** is an external crate pulled via `path = "../../boolmesh/boolmesh"`
  (not a git dependency). The submodule must exist for `interpreter` to build.
- **GUI requires Linux/Wayland** and links against Wayland, X11, Vulkan, ALSA
  etc. It won't cross-compile cleanly on non-Linux hosts.
- **CLI stores project state** in `.ccad/store/` (discovered via git root).
- **Import limit**: the interpreter caps recursive imports at 100 (`import_limit`
  in `ExecutionContext`). See `interpreter/test_assets/infinite_recursion_import.ccm`.
- **Editions**: `gui` and `cli` use Rust 2024; others use 2021.
