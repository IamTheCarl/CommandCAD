{
  description = "A data driven CAD program";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    fenix = {
      # url = "github:nix-community/fenix";
      url = "github:nix-community/fenix?ref=eureka-cpu/rust-analyzer-wrapped";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    crane,
    fenix,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        fenix-pkgs = fenix.packages.${system};
        fenix-channel = fenix-pkgs.stable;
        fenix-toolchain = fenix-channel.withComponents [
          "cargo"
          "clippy"
          "rust-src"
          "rustc"
          "rustfmt"
          "rust-analyzer"
        ];
        craneLib = (crane.mkLib pkgs).overrideScope (final: prev: {
          cargo = fenix-channel.cargo;
          rustc = fenix-channel.rustc;
        });
      in rec
      {
	packages.default = with pkgs; craneLib.buildPackage {
          nativeBuildInputs = [
            openssl
	    pkg-config
          ];

	  buildInputs = [
            openssl
	  ];

          src = craneLib.cleanCargoSource ./.;
          strictDeps = true;
        };

        devShells.default = with pkgs; pkgs.mkShell {
          inputsFrom = [ packages.default ];
          buildInputs = [
	    bashInteractive
	    nodejs_24
            tree-sitter
            fenix-toolchain
            cargo-expand
            openssl
	    pkg-config
          ];

          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            openssl
          ];

	  shellHook = ''
            export SHELL=${pkgs.bashInteractive}/bin/bash
            export NIX_HARDENING_ENABLE=""
          '';
        };

      }
    );
}
