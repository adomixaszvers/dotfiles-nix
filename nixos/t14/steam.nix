{ pkgs, ... }: {
  programs = {
    steam = {
      enable = true;
      package = pkgs.steam.override { extraPkgs = ps: with ps; [ mangohud ]; };
    };
    gamescope.enable = true;
  };
}
