{
  nixpkgs.config = {
    nvidia.acceptLicense = true;
    steam.primus = true;
  };
  hardware.bumblebee = {
    enable = true;
    driver = "nvidia";
    pmMethod = "bbswitch";
  };
  nixpkgs.overlays = [
    (_self: super: {
      bumblebee = super.bumblebee.overrideAttrs (oldAttrs: {
        meta = oldAttrs.meta // {
          broken = false;
        };
      });
      linuxPackages = super.linuxPackages_6_1.extend (
        _lpself: lpsuper: { nvidia_x11 = lpsuper.nvidia_x11_legacy390; }
      );
      steam = super.steam.override {
        extraPkgs = pkgs: [
          pkgs.bumblebee
          pkgs.glxinfo
        ];
      };
    })
  ];
}
