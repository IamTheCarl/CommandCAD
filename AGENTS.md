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

## Implicit surface rendering

Ray-marched 3D implicit surfaces, rendered via GPU shaders generated from
fidget `Tree` SDF expressions. Two-pass architecture:

1. **Main pass** (`ImplicitMainPassNode`): fullscreen fragment shader that
   ray-marches the SDF per-pixel, writes color + depth to intermediate
   `Rgba16Float` textures. Includes SDF Near-Miss outline detection in the
   same pass (no extra sampling needed).
2. **Writeback pass** (`ImplicitWritebackNode`): composites intermediate
   results onto main render target with depth testing, inserted between
   `MainOpaquePass` and `MainTransmissivePass` in the render graph.

### Key files

| File | Role |
|---|---|
| `gui/src/visualizers/implicit3d.rs` | Full render pipeline: plugin, nodes, resources, shader generation (~825 lines) |
| `gui/src/tree_to_wgsl.rs` | WGSL codegen: `tree_to_wgsl_main_from_sdf()`, `tree_to_wgsl_writeback()`, `emit_sdf_body()` |
| `interpreter/src/execution/values/implicit_surface/surface3d.rs` | `Surface3D` type, SDF construction for built-in shapes (cone, cylinder, sphere, torus) |

### Shader generation flow

1. `update_implicit_shader` runs on main world after `check_job`, triggered when
   `JobBridge.last_result` is `Ok(JobOutput::Surface3D(...))`.
2. Calls `emit_sdf_body(surface.tree())` to convert fidget `Tree` → WGSL expression.
3. Wraps in `tree_to_wgsl_main_from_sdf()` to produce full fragment shader.
4. Registers with Bevy as `Shader::from_wgsl(...)`, caches handle in
   `JobBridge.implicit_shader` and `ImplicitFragmentShader` resource.
5. Increments `ImplicitShaderVersion` so render world re-specializes pipelines.

### Uniform buffer (`ImplicitUniform`)

Layout in `visualizers/implicit3d.rs` must match WGSL struct in
`tree_to_wgsl.rs` (`MAIN_UNIFORM_HEADER`). Fields: camera position, right/up/forward
axes, ortho half-width/height, and `viewport_size` (pixel dimensions). The
`viewport_size` is used to compute `world_units_per_pixel` for zoom-scaled epsilon
and outline threshold.

### SDF Near-Miss outline

Single-pass outline algorithm (no 2D screen-space edge detection). During
raymarching, tracks minimum SDF value (`nearest`) and ray distance at near-miss
(`nearest_t`). On miss, computes outline intensity:
`1.0 - pow(clamp(nearest / threshold, 0, 1), 8)`. Threshold = `2.0 * world_units_per_pixel`
(~2-pixel screen width). Outline color is white. Depth = `-(nearest_t + nearest)`
for proper compositing.

### Ray-marching epsilon

Scales with zoom: `epsilon = 0.5 * world_units_per_pixel`. Maintains sub-pixel
precision at any zoom level. Computed once before the raymarching loop.

### Lighting

Camera-following directional light: `-camera_forward` as light direction, matching
Bevy's orbiting `DirectionalLight`. Base color 0.502 (gray) with PBR-style
ambient + diffuse: `base_color * (0.05 + 0.95 * max(dot(n, light_dir), 0))`.

### SDF normalization

All built-in SDFs use normalized formulas where |grad| = 1 everywhere, ensuring
correct finite-difference normals and lighting. Cone uses `(b*h - r*a) / sqrt(h²+r²)`
where `a = z + h/2` (height from apex), `b = sqrt(x²+y²)` (radial distance).
