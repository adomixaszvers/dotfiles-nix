{ pkgs ? import <nixpkgs> { } }:
let
  extraPackages = import ./extraPackages.nix;
  all-hies = import <all-hies> { };
  hie = all-hies.selection { selector = p: { inherit (p) ghc865; }; };
in with pkgs;

mkShell {
  buildInputs = [
    (ghc.withHoogle extraPackages)
    hie
    haskellPackages.brittany
    haskellPackages.hlint
  ];
}
