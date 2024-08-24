{ inputs ? (builtins.getFlake "mine").inputs, system ? builtins.currentSystem }:
let
  inherit (inputs) nixpkgs;
  xmonad-flake = inputs.xmonad;
  xmonad-contrib-flake = inputs.xmonad-contrib;
  pkgs = import nixpkgs {
    inherit system;
    overlays = [ xmonad-flake.overlay xmonad-contrib-flake.overlay ];
  };
in with pkgs;
mkShellNoCC {
  name = "xmonad-shell";
  buildInputs = [
    (haskellPackages.ghcWithHoogle (hs:
      (import ./extraPackages.nix hs)
      ++ [ (hs.callPackage ./my-colors.nix { }) ]))
    haskellPackages.haskell-language-server
  ];
}
