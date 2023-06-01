{ pkgs ?
  (builtins.getFlake "nixpkgs").legacyPackages."${builtins.currentSystem}" }:
with pkgs;
mkShell {
  name = "xmonad-shell";
  buildInputs = [
    (haskellPackages.ghcWithHoogle (hs:
      (import ./extraPackages.nix hs)
      ++ [ (hs.callPackage ./my-colors.nix { }) ]))
    haskellPackages.haskell-language-server
  ];
}
