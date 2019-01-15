{ pkgs, config, ... }:
with config.lib.colors.solarized;
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3GapsSupport = true;
      alsaSupport = true;
      pulseSupport = true;
    };
    extraConfig = import ./extraConfig.nix {inherit pkgs; colors = config.lib.colors.solarized;};
    script = import ./scripts/launch.nix;
  };
}
