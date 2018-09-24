[(self: super:
{
  fetchNixPkgs = { rev, sha256 }:
  builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  unstablePkgs = import (self.fetchNixPkgs {
    rev = "3cee0ce5b8705df5828a2ef247aa58781237e953";
    sha256 = "0wrnk3r93467qji7xdjd80h935djr9cr2w5n7k15lw5m154ca60f";
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
