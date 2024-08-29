{
  pkgs ? import <nixpkgs> { },
}:
with pkgs;
mkShellNoCC {
  name = "xmonad-shell";
  buildInputs =
    let
      myHaskellPackages = import ./myHaskellPackages.nix { inherit haskellPackages lib haskell; };
    in
    [
      (myHaskellPackages.ghcWithHoogle (
        hs: (import ./extraPackages.nix hs) ++ [ (hs.callPackage ./my-colors.nix { }) ]
      ))
      haskellPackages.haskell-language-server
    ];
}
