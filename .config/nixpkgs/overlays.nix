[(self: super:
{
  fetchNixPkgs = { rev, sha256 }:
  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  unstablePkgs = import (self.fetchNixPkgs {
    rev = "9fa6a261fb237f68071b361a9913ed1742d5e082";
    sha256 = "11733y8xfbisvp8jzpcpjwz70883qfnlzdxv7yl3k2accin88a9z";
  }) { config = self.config; };
  mine = {
    consul = super.callPackage ./pkgs/consul {};
    bumblebee-status = super.callPackage ./pkgs/bumblebee-status {};
    vimgolf = super.callPackage ./pkgs/vimgolf {};
    ghc = let
      haskellPackages = ps: with ps; [
        # ghc-mod
        hasktags
        hdevtools
        hindent
        hlint
        hoogle
        hspec
        pointfree pointful
        stylish-haskell
      ];
    in
    (self.ghc.withPackages haskellPackages);
  };
})]
