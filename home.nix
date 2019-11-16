{ pkgs, config, lib, ... }: {
  imports = let
    current = ./hosts/current.nix;
    hostSpecific = if builtins.pathExists current then current else { };
  in [ hostSpecific ];
  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];
  home.stateVersion = "19.09";
  manual.html.enable = true;
  programs.home-manager.enable = true;
  # programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-19.03.tar.gz;
  systemd.user = {
    startServices = true;
    systemctlPath = "/run/current-system/sw/bin/systemctl";
  };

  xdg = { enable = true; };
}
