{ pkgs, ... }: {
  imports = let
    current = ./hosts/current.nix;
    hostSpecific = if builtins.pathExists current then current else { };
  in [ hostSpecific ];
  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];
  home.stateVersion = "19.09";
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
    path = pkgs.nivSources.home-manager.outPath;
  };
  systemd.user = { startServices = true; };

  xdg = { enable = true; };
}
