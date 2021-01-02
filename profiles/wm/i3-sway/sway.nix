{ pkgs, config, lib, ... }:
let common = import ./common.nix { inherit pkgs config; };
in {
  wayland.windowManager.sway = {
    enable = true;
    config = {
      inherit (common.config)
        bars colors fonts gaps modifier assigns window keycodebindings;
      keybindings = let
        modifier = common.config.modifier;
        combined = common.config.keybindings // {
          "${modifier}+d" = "exec ${pkgs.wofi}/bin/wofi --show drun,run";
        };
      in lib.mkOptionDefault combined;
      input = { "*" = { xkb_layout = "lt,us"; }; };
    };
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
    '';
  };
}
