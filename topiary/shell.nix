{ pkgs ? import <nixpkgs> { } }:
let
  topiary = pkgs.rustPlatform.buildRustPackage rec {
    pname = "topiary-cli";
    version = "0.5.1";

    src = pkgs.fetchCrate {
      inherit pname version;
      sha256 = "sha256-D8P4LHq8DxDH2o2aWJjDmMnx+aDlcb0zjeVcUry3jrs=";
    };

    cargoSha256 = "sha256-5c88uUZ3USeRQ6QY/Qfax5UbLsAX6xswybLPhybCJko=";

    # Checks require network access to download languages.
    doCheck = false;
  };
in
pkgs.mkShell {
  buildInputs = [
    topiary
  ];

  # TODO make this point to the directory with our language query.  
  shellHook = ''
    # export TOPIARTY_LANGUAGE_DIR=
  '';
}
