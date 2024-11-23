{ pkgs }:
let
  rust-overlay = import (builtins.fetchTarball "https://github.com/oxalica/rust-overlay/archive/stable.tar.gz");
  pkgs = import <nixpkgs> { overlays = [ rust-overlay ]; };
  rust = pkgs.rust-bin.stable.latest.default.override {
    extensions = [
      "rust-src"
      "rust-analyzer"
      "rustfmt"
      "clippy"
    ];
    targets = [
      "x86_64-unknown-linux-gnu"
    ];
  };
  rust_platform = pkgs.makeRustPlatform {
    cargo = rust;
    rustc = rust;
  };

  tree-sitter-cli = rust_platform.buildRustPackage rec {
    pname = "tree-sitter-cli";
    version = "0.24.4";

    src = pkgs.fetchCrate {
      inherit pname version;
      sha256 = "sha256-+45nWB37pR6yUqyFovlnOTCCHSPPeoKa0mcEYQn2I1o=";
    };

    cargoSha256 = "sha256-WDMAuRbU7Afuwg4D9oZG2ZIQfhkFXZXhCnJ3qqZyuSo=";
    doCheck = false;
  };
in
[
  rust
  rust_platform.bindgenHook
  tree-sitter-cli
  pkgs.crate2nix
  pkgs.openssl
  pkgs.pkg-config
  pkgs.nodejs_22
  pkgs.gcc
]
