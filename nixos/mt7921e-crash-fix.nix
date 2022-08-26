{
  boot.kernelPatches = [{
    name = "mt7921e-fix-crash-in-chip-reset-fail";
    patch = builtins.fetchurl {
      url =
        "https://patchwork.kernel.org/project/linux-mediatek/patch/727eb5ffd3c7c805245e512da150ecf0a7154020.1659452909.git.deren.wu@mediatek.com/raw/";
      sha256 = "06i6i5m9gkv85pp16b1jv1ylapx5liid2ymnrgrics4idbwb6xnc";
    };
  }];
}
