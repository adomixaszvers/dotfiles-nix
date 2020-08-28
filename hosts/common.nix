{ pkgs, ... }: {
  home.extraOutputsToInstall = [ "doc" "info" "devdoc" ];
  home.homeDirectory = "/home/adomas";
  home.stateVersion = "20.03";
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
  };
  systemd.user = { startServices = true; };

  xdg = { enable = true; };
}
