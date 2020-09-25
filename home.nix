{ pkgs, ... }: {
  imports = let
    current = ./current.nix;
    hostSpecific = if builtins.pathExists current then current else { };
  in [ ./modules hostSpecific ];
  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];
  home.stateVersion = "20.03";
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
    path = pkgs.nivSources.home-manager.outPath;
  };
  systemd.user = { startServices = true; };

  xdg = { enable = true; };
}
