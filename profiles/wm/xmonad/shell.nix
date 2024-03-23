{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShellNoCC {
  name = "xmonad-shell";
  buildInputs = [
    (haskellPackages.ghcWithHoogle (hs:
      (import ./extraPackages.nix hs)
      ++ [ (hs.callPackage ./my-colors.nix { }) ]))
    haskellPackages.haskell-language-server
  ];
}
