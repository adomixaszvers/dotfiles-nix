self: super:
{
  mine = {
    consul = super.callPackage ../pkgs/consul {};
    bumblebee-status = super.callPackage ../pkgs/bumblebee-status {};
    vimgolf = super.callPackage ../pkgs/vimgolf {};
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
}
