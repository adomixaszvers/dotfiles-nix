{
  hardware.bumblebee = {
    enable = true;
    driver = "nvidia";
    pmMethod = "bbswitch";
  };
  nixpkgs.overlays = [
    (_self: super: {
      linuxPackages = super.linuxPackages.extend
        (_lpself: lpsuper: { nvidia_x11 = lpsuper.nvidia_x11_legacy390; });
    })
  ];
}
