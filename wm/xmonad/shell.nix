{ pkgs ? import <nixpkgs> { } }:
let
  extraPackages = import ./extraPackages.nix;
  all-hies = import <all-hies> { };
  hie = all-hies.latest;
in with pkgs;

mkShell {
  buildInputs = [
    (ghc.withHoogle extraPackages)
    hie
    haskellPackages.brittany
    haskellPackages.hlint
  ];
}
