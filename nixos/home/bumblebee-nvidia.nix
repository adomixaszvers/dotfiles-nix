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
      steam = super.steam.override {
        withPrimus = true;
        extraPkgs = pkgs: [ pkgs.bumblebee pkgs.glxinfo ];
      };
    })
  ];
}
