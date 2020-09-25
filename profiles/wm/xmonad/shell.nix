{ pkgs ? import <nixpkgs> { } }:
let extraPackages = import ./extraPackages.nix;
in with pkgs;

mkShell {
  buildInputs = [
    (ghc.withHoogle extraPackages)
    haskellPackages.hie
    haskellPackages.brittany
    haskellPackages.hlint
  ];
}
