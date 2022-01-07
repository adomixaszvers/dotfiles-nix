{ pkgs, config, lib, ... }:
let
  common = import ./common.nix { inherit config; };
  wofi-windows = pkgs.writeShellScript "wofi-windows" ''
    PATH=${lib.makeBinPath (with pkgs; [ sway jq wofi ])}
    swaymsg -t get_tree |
         jq  -r  '.nodes[].nodes[]  |  if  .nodes  then  [recurse(.nodes[])] else [] end +
       .floating_nodes | .[] | select(.nodes==[]) | ((.id | tostring) + "" + .name)' |
         wofi --show dmenu | {
           read -r id name
           swaymsg "[con_id=$id]" focus
       }
  '';
in {
  imports = [ ./i3status-rust.nix ];
  home.packages = with pkgs; [ wofi ];
  programs.mako.enable = true;
  wayland.windowManager.sway = {
    enable = true;
    config = {
      inherit (common.config)
        bars colors fonts gaps modifier assigns window keycodebindings;
      keybindings = let
        inherit (common.config) modifier;
        combined = common.config.keybindings // {
          "${modifier}+d" = "exec ${pkgs.wofi}/bin/wofi --show drun";
          "${modifier}+Shift+d" = "exec ${pkgs.wofi}/bin/wofi --show run";
          "${modifier}+Tab" = "exec ${wofi-windows}";
        };
      in lib.mkOptionDefault combined;
      startup = [
        { command = "mako"; }
        { command = "${pkgs.autotiling}/bin/autotiling"; }
      ];
    };
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
      export BROWSER=firefox
    '';
    wrapperFeatures.gtk = true;
  };
}
