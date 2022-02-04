{ config, pkgs, ... }: {
  hardware.bumblebee = {
    enable = true;
    driver = "nvidia";
    pmMethod = "bbswitch";
  };
  nixpkgs.overlays = [
    (self: super: {
      linuxPackages = super.linuxPackages.extend
        (lpself: lpsuper: { nvidia_x11 = lpsuper.nvidia_x11_legacy390; });
    })
  ];
}
