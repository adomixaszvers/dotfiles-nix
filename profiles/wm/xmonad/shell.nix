{ pkgs ?
  (builtins.getFlake "nixpkgs-hs").legacyPackages."${builtins.currentSystem}" }:
let
  extraPackages = import ./extraPackages.nix;
  colors = import ./my-colors.nix;
in with pkgs;
mkShell {
  buildInputs = let
    myHaskellPackages = pkgs.haskellPackages.override {
      overrides = self: _super: { my-colors = self.callPackage colors { }; };
    };
  in [
    (myHaskellPackages.ghcWithHoogle
      (hs: (extraPackages hs) ++ [ hs.my-colors ]))
    haskellPackages.haskell-language-server
  ];
}
