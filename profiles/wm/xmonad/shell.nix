{ xmonad-flake ? import <xmonad> { }
, xmonad-contrib-flake ? import <xmonad-contrib> { }, pkgs ? import <nixpkgs> {
  overlays = [ xmonad-flake.overlay xmonad-contrib-flake.overlay ];
} }:
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
