{
  haskellPackages,
  haskell,
  lib,
}:
haskellPackages.override {
  overrides = _: super: {
    xmonad = super.xmonad_0_18_0;
    xmonad-contrib = haskell.lib.compose.overrideCabal (_drv: {
      version = "0.18.1";
      sha256 = "0ck4hq9yhdzggrs3q4ji6nbg6zwhmhc0ckf9vr9d716d98h9swq5";
    }) super.xmonad-contrib;
  };
}
