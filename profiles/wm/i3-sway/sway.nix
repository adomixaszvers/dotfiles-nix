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
  home.packages = (with pkgs; [ wofi swaylock swayidle wl-clipboard ])
    ++ [ myPkgs.sway-greedy-focus ];
  programs.mako.enable = true;
  wayland.windowManager.sway = {
    enable = true;
    config = {
      inherit (common.config) bars colors fonts gaps modifier assigns;
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
      in combined;
      keycodebindings = let
        inherit (common.config) modifier;
        combined = common.config.keycodebindings // {
          "${modifier}+10" = "exec sway-greedy-focus 1";
          "${modifier}+11" = "exec sway-greedy-focus 2";
          "${modifier}+12" = "exec sway-greedy-focus 3";
          "${modifier}+13" = "exec sway-greedy-focus 4";
          "${modifier}+14" = "exec sway-greedy-focus 5";
          "${modifier}+15" = "exec sway-greedy-focus 6";
          "${modifier}+16" = "exec sway-greedy-focus 7";
          "${modifier}+17" = "exec sway-greedy-focus 8";
          "${modifier}+18" = "exec sway-greedy-focus 9";
          "${modifier}+19" = "exec sway-greedy-focus 10";
        };
      in combined;
      startup = [
        { command = "mako"; }
        { command = "${pkgs.autotiling}/bin/autotiling"; }
      ];
      window = lib.mkMerge [
        common.config.window
        {
          commands = [
            {
              criteria = { app_id = "emacs_scratch"; };
              command = "move scratchpad, fullscreen enable, scratchpad show";
            }
            {
              criteria = { app_id = "kitty_scratch"; };
              command = "move scratchpad, fullscreen enable, scratchpad show";
            }
          ];
        }
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
