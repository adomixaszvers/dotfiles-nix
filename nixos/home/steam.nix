{
  nixpkgs.config.packageOverrides = pkgs: {
    steam = pkgs.steam.override {
      withPrimus = true;
      extraPkgs = pkgs: [ pkgs.bumblebee pkgs.glxinfo ];
    };
  };
  programs.steam.enable = true;
}
