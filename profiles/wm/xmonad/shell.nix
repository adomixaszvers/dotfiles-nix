{ pkgs ? (builtins.getFlake (toString
  ~/.config/nixpkgs)).inputs.nixos-unstable.legacyPackages."${builtins.currentSystem}"
}:
let
  extraPackages = import ./extraPackages.nix;
  colors = import ./my-colors.nix;
in with pkgs;
mkShell {
  buildInputs = let
    myHaskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        my-colors = self.callPackage colors { };
        xmonad = super.xmonad_0_17_0;
        xmonad-contrib = super.xmonad-contrib_0_17_0;
      };
    };
  in [
    (myHaskellPackages.ghcWithHoogle
      (hs: (extraPackages hs) ++ [ hs.my-colors ]))
    haskellPackages.haskell-language-server
  ];
}
