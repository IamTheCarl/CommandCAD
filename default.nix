{ pkgs ? import <nixpkgs> { } }:
let
  addDeps = list: { ... }: {
    nativeBuildInputs = list ++ (import ./build_dependencies.nix {
      pkgs = pkgs;
    });
  };
  cargo_nix = pkgs.callPackage ./Cargo.nix { };
in
cargo_nix.workspaceMembers."command_cad".build.overrideAttrs { }
