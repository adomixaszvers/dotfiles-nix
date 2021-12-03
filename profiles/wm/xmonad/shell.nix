{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
let extraPackages = import ./extraPackages.nix;
in with pkgs;

mkShell {
  buildInputs =
    [ (ghc.withHoogle extraPackages) haskellPackages.haskell-language-server ];
}
