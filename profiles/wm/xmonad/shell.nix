let
  haskellOverrides = _: prev: {
    haskellPackages = prev.haskellPackages.override {
      overrides = _: hsOld: {
        xmonad-dbus = with prev.haskell.lib;
          unmarkBroken (dontCheck hsOld.xmonad-dbus);
      };
    };
  };
in { pkgs ? import (builtins.getFlake "nixpkgs") {
  overlays = [ haskellOverrides ];
  system = builtins.currentSystem;
} }:
let extraPackages = import ./extraPackages.nix;
in with pkgs;

mkShell {
  buildInputs =
    [ (ghc.withHoogle extraPackages) haskellPackages.haskell-language-server ];
}
