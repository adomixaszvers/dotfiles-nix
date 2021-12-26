{ inputs, ... }: {
  imports = [ ../modules ./cli ./gui ./wm/common.nix ];
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
    path = inputs.home-manager.outPath;
  };
  systemd.user = { startServices = true; };

  xdg = { enable = true; };
}
