{ pkgs, config, lib, myPkgs, ... }:
let
  common = import ./common.nix { inherit config; };
  wofi-powermenu = pkgs.writeShellScript "wofi-powermenu" ''
    PATH=$PATH:${lib.makeBinPath [ pkgs.wofi pkgs.procps ]}

    wofi-question () {
      local question=$1
      [ $(printf "No\\nYes" | wofi -dmenu -L 2 -i -p "$question") == Yes ]
    }

    case "$(printf "lock session\\nlogout\\npoweroff\\nreboot"| wofi -dmenu -L 4 -i -p "Power menu")" in
      "lock session") pkill -USR1 swayidle ;;
      "logout") wofi-question "Are you sure you want to logout?" && loginctl kill-session $XDG_SESSION_ID ;;
      "poweroff") wofi-question "Are you sure you want to power off the computer?" && systemctl poweroff ;;
      "reboot") wofi-question "Are you sure you want to reboot the computer?" && systemctl reboot ;;
    esac
  '';
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
  imports = [ ../waybar ];
  home.packages =
    (with pkgs; [ wofi swaylock swayidle wl-clipboard font-awesome_5 ])
    ++ [ myPkgs.sway-greedy-focus ];
  programs.mako.enable = true;
  programs.waybar = {
    enable = true;
    settings = {
      "sway/workspaces" = {
        disable-scroll = true;
        all-outputs = false;
      };
      "clock" = { format = "{:%Y-%m-%d %H:%M}"; };
      "pulseaudio" = { scroll-step = "5.0"; };
      temperature.thermal-zone = 1;
      mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [ "sway/workspaces" "sway/mode" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "pulseaudio" "cpu" "memory" "temperature" "clock" ];
      };
    };
  };
  services.kanshi.enable = true;
  wayland.windowManager.sway = {
    enable = true;
    config = {
      inherit (common.config) colors fonts gaps modifier assigns;
      keybindings = let
        inherit (common.config) modifier;
        combined = common.config.keybindings // {
          "${modifier}+F4" = "exec ${wofi-powermenu}";
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
      bars = [{ command = "waybar"; }];
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
    '';
    wrapperFeatures.gtk = true;
  };
}
