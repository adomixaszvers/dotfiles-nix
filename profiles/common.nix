{ inputs, ... }: {
  imports = [ ./cli ./gui ./wm/common.nix ];
  manual.html.enable = true;
  programs.home-manager = {
    enable = true;
    path = inputs.home-manager.outPath;
  };
  systemd.user.startServices = "sd-switch";

  xdg.enable = true;
}
