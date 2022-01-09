{ pkgs, config, lib, myPkgs, ... }:
let
  common = import ./common.nix { inherit config; };
  wofi-windows = pkgs.writeShellScript "wofi-windows" ''
        PATH=${lib.makeBinPath (with pkgs; [ sway jq wofi ])}
    		swaymsg -t get_tree | jq -r '
            # descend to workspace or scratchpad
            .nodes[].nodes[]
            # save workspace name as .w
            | {"w": .name} + (
                    if .nodes then # workspace
                            [recurse(.nodes[])]
                    else # scratchpad
                            []
                    end
                    + .floating_nodes
                    | .[]
                    # select nodes with no children (windows)
                    | select(.nodes==[])
            )
            | ((.id | tostring) + "\t "
            # remove markup and index from workspace name, replace scratch with "[S]"
            + (.w | gsub("^[^:]*:|<[^>]*>"; "") | sub("__i3_scratch"; "[S]"))
            + "\t " +  .name)
            ' | wofi --show dmenu --prompt='Focus a window' | {
        read -r id name
        swaymsg "[con_id=$id]" focus
    }
      '';
in {
  imports = [ ./i3status-rust.nix ];
  home.packages = with pkgs; [ wofi swaylock swayidle wl-clipboard ];
  programs.mako.enable = true;
  wayland.windowManager.sway = {
    enable = true;
    config = {
      inherit (common.config) bars colors fonts gaps modifier assigns window;
      keybindings = let
        inherit (common.config) modifier;
        combined = common.config.keybindings // {
          "${modifier}+d" = "exec ${pkgs.wofi}/bin/wofi --show drun";
          "${modifier}+Shift+d" = "exec ${pkgs.wofi}/bin/wofi --show run";
          "${modifier}+Tab" = "exec ${wofi-windows}";
          "${modifier}+Ctrl+e" = ''
            exec swaymsg "[app_id=emacs_scratch] scratchpad show, fullscreen enable" || exec emacs --name emacs_scratch'';
          "${modifier}+Ctrl+s" = ''
            exec swaymsg "[app_id=kitty_scratch] scratchpad show, fullscreen enable" || exec kitty --class kitty_scratch'';
        };
      in lib.mkOptionDefault combined;
      keycodebindings = let
        inherit (common.config) modifier;
        combined = common.config.keycodebindings // {
          "${modifier}+10" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 1";
          "${modifier}+11" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 2";
          "${modifier}+12" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 3";
          "${modifier}+13" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 4";
          "${modifier}+14" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 5";
          "${modifier}+15" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 6";
          "${modifier}+16" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 7";
          "${modifier}+17" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 8";
          "${modifier}+18" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 9";
          "${modifier}+19" =
            "exec ${myPkgs.sway-greedy-focus}/bin/sway-greedy-focus 10";
        };
      in lib.mkOptionDefault combined;
      startup = [
        { command = "mako"; }
        { command = "${pkgs.autotiling}/bin/autotiling"; }
      ];
    };
    extraConfig = ''
      for_window [app_id="emacs_scratch"] move scratchpad, fullscreen enable, scratchpad show
      for_window [app_id="kitty_scratch"] move scratchpad, fullscreen enable, scratchpad show
    '';
    extraSessionCommands = ''
      export MOZ_ENABLE_WAYLAND=1
      export _JAVA_AWT_WM_NONREPARENTING=1
      export BROWSER=firefox
    '';
    wrapperFeatures.gtk = true;
  };
}
