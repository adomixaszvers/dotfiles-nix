{ pkgs ? import <nixpkgs> { } }:
let
  extraPackages = import ./extraPackages.nix;
  all-hies = import (fetchTarball
    "https://github.com/rycee/home-manager/archive/release-19.09.tar.gz") { };
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
in with pkgs;

mkShell {
  buildInputs = [
    (ghc.withHoogle extraPackages)
    # hie
    haskellPackages.brittany
    haskellPackages.hlint
  ];
}
