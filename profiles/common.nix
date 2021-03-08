{ pkgs, ... }: {
  imports = [ ../modules ./cli ./gui ./wm/common.nix ];
  home = {
    stateVersion = "20.09";
    username = "adomas";
    homeDirectory = "/home/adomas";
  };
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
    path = pkgs.inputs.home-manager.outPath;
  };
  systemd.user = { startServices = true; };

  xdg = { enable = true; };
}
