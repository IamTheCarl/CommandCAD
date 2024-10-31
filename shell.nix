{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = (import ./build_dependencies.nix {
    pkgs = pkgs;
  });

  # Set environment variables
  shellHook = ''
    export OPENSSL_NO_VENDOR=1
  '';
}
