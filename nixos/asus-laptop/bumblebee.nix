{
  nixpkgs.overlays = [
    (_: prev: {
      kernelPackagesExtensions = prev.kernelPackagesExtensions ++ [
        (_: linuxPrev: {
          bbswitch = linuxPrev.callPackage ./bbswitch { };
        })
      ];
    })
  ];
  hardware.bumblebee = {
    enable = true;
    driver = "nouveau";
    pmMethod = "bbswitch";
  };
}
