{ pkgs ? (builtins.getFlake (toString
  ~/.config/nixpkgs)).inputs.nixos-unstable.legacyPackages."${builtins.currentSystem}"
}:
let extraPackages = import ./extraPackages.nix;
in with pkgs;

mkShell {
  buildInputs = let
    myHaskellPackages = pkgs.haskellPackages.override {
      overrides = _: super: {
        xmonad = super.xmonad_0_17_0;
        xmonad-contrib = super.xmonad-contrib_0_17_0;
      };
    };
  in [
    (myHaskellPackages.ghcWithHoogle extraPackages)
    haskellPackages.haskell-language-server
  ];
}
