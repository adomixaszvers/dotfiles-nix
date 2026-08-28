{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShellNoCC {
  name = "xmonad-shell";
  packages =
    let
      myHaskellPackages = import ./myHaskellPackages.nix { inherit (pkgs) haskellPackages; };
    in
    [
      (myHaskellPackages.ghcWithHoogle (
        hs: (import ./extraPackages.nix hs) ++ [ (hs.callPackage ./my-colors.nix { }) ]
      ))
      pkgs.haskellPackages.haskell-language-server
    ];
}
