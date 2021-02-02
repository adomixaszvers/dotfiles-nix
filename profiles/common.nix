{ pkgs, ... }: {
  imports = [ ../modules ./cli ./gui ./wm/common.nix ];
  home = {
    extraOutputsToInstall = [ "doc" "info" "devdoc" ];
    stateVersion = "20.09";
  };
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
    path = pkgs.nivSources.home-manager.outPath;
  };
  systemd.user = { startServices = true; };

  xdg = { enable = true; };
}
