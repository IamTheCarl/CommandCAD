{
  description = "A data driven CAD program";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      crane,
      fenix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
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
        craneLib = (crane.mkLib pkgs).overrideScope (
          final: prev: {
            cargo = fenix-channel.cargo;
            rustc = fenix-channel.rustc;
          }
        );

        core-dependencies = with pkgs; [
          bashInteractive
          nodejs_24
          tree-sitter
          fenix-toolchain
          cargo-expand
          openssl
          pkg-config
        ];

        gui-dependencies = with pkgs; [
          wayland
          libxkbcommon
          libX11
          libXcursor
          libXi
          vulkan-loader
          libGL
          alsa-lib
          udev
          mesa
          libglvnd
        ];
      in
      {
        packages.default =
          with pkgs;
          craneLib.buildPackage {
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

        devShells = with pkgs; rec {
          common = mkShell {
            buildInputs = core-dependencies;

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              openssl
            ];

            shellHook = ''
              export SHELL=${pkgs.bashInteractive}/bin/bash
              export NIX_HARDENING_ENABLE=""
            '';
          };

          gui = mkShell {
            buildInputs = core-dependencies ++ gui-dependencies;

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath (
              [
                openssl
              ]
              ++ gui-dependencies
            );

            shellHook = ''
              export SHELL=${pkgs.bashInteractive}/bin/bash
              export NIX_HARDENING_ENABLE=""
              export LD_LIBRARY_PATH="${mesa.outPath}/lib:${libglvnd.outPath}/lib:$LD_LIBRARY_PATH"
              export LIBGL_DRIVERS_PATH="${mesa.outPath}/lib/dri"
              export GBM_BACKENDS_PATH="${mesa.outPath}/lib/gbm"
              export __EGL_VENDOR_LIBRARY_FILENAMES="${mesa.outPath}/share/glvnd/egl_vendor.d/50_mesa.json"
              export VK_ICD_FILENAMES="${mesa.outPath}/share/vulkan/icd.d/radeon_icd.x86_64.json:${mesa.outPath}/share/vulkan/icd.d/intel_icd.x86_64.json:${mesa.outPath}/share/vulkan/icd.d/nouveau_icd.x86_64.json"
              export VK_LAYER_PATH="${mesa.outPath}/share/vulkan/implicit_layer.d"
              export LIBVA_DRIVERS_PATH="${mesa.outPath}/lib/dri"
              # nixGL wrapper for Vulkan-on-OpenGL rendering on SteamOS
              nixGL() {
                export LD_LIBRARY_PATH="${mesa.outPath}/lib:${libglvnd.outPath}/lib:$LD_LIBRARY_PATH"
                export LIBGL_DRIVERS_PATH="${mesa.outPath}/lib/dri"
                export GBM_BACKENDS_PATH="${mesa.outPath}/lib/gbm"
                export __EGL_VENDOR_LIBRARY_FILENAMES="${mesa.outPath}/share/glvnd/egl_vendor.d/50_mesa.json"
                export VK_ICD_FILENAMES="${mesa.outPath}/share/vulkan/icd.d/radeon_icd.x86_64.json:${mesa.outPath}/share/vulkan/icd.d/intel_icd.x86_64.json:${mesa.outPath}/share/vulkan/icd.d/nouveau_icd.x86_64.json"
                export VK_LAYER_PATH="${mesa.outPath}/share/vulkan/implicit_layer.d"
                export LIBVA_DRIVERS_PATH="${mesa.outPath}/lib/dri"
                exec "$@"
              }
            '';
          };

          default = gui;
        };
      }
    );
}
