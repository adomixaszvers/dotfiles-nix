{
  pkgs,
  config,
  lib,
  myPkgs,
  ...
}:
let
  rofi = config.programs.rofi.finalPackage;
  common = import ./common.nix { inherit config; };
  rofi-powermenu = pkgs.writeShellScript "rofi-powermenu" ''
    PATH=$PATH:${
      lib.makeBinPath [
        rofi
        pkgs.procps
      ]
    }

    rofi-question () {
      local question=$1
      [ $(printf "No\\nYes" | rofi -dmenu -L 2 -i -p "$question") == Yes ]
    }

    case "$(printf "lock session\\nlogout\\npoweroff\\nreboot"| rofi -dmenu -L 4 -i -p "Power menu")" in
      "lock session") pkill -USR1 swayidle ;;
      "logout") rofi-question "Are you sure you want to logout?" && loginctl kill-session $XDG_SESSION_ID ;;
      "poweroff") rofi-question "Are you sure you want to power off the computer?" && systemctl poweroff ;;
      "reboot") rofi-question "Are you sure you want to reboot the computer?" && systemctl reboot ;;
    esac
  '';
  rofi-windows = pkgs.writeShellScript "rofi-windows" ''
        PATH=${
          lib.makeBinPath (
            with pkgs;
            [
              sway
              jq
              rofi
            ]
          )
        }
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
            ' | rofi -dmenu --prompt='Focus a window' | {
        read -r id name
        swaymsg "[con_id=$id]" focus
    }
  '';
in
{
  imports = [
    ../dunst.nix
    ../waybar
  ];
  home.packages =
    (with pkgs; [
      pamixer
      wl-clipboard
      font-awesome_5
      wdisplays
    ])
    ++ [ myPkgs.sway-greedy-focus ];
  programs = {
    emacs.package = pkgs.emacs-pgtk;
    rofi = {
      package = pkgs.rofi-wayland;
      extraConfig.modi = "drun,run,ssh";
    };
    waybar = {
      settings.mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [
          "sway/workspaces"
          "sway/mode"
        ];
        modules-right = [
          "pulseaudio"
          "cpu"
          "memory"
          "temperature"
          "clock"
          "tray"
        ];
        "clock" = {
          format = "{:%Y-%m-%d %H:%M}";
        };
        "pulseaudio" = {
          scroll-step = "5.0";
        };
        temperature.thermal-zone = config.gui.thermal-zone;
      };
      systemd = {
        enable = true;
        target = "sway-session.target";
      };
    };
  };
  services = {
    kanshi.enable = true;
    dunst.waylandDisplay = "wayland-1";
    volnoti.enable = true;
  };
  stylix.targets.sway.enable = true;
  wayland.windowManager.sway = {
    enable = true;
    config = {
      inherit (common.config)
        bars
        fonts
        gaps
        modifier
        assigns
        ;
      keybindings =
        let
          inherit (common.config) modifier;
          combined = common.config.keybindings // {
            "${modifier}+F4" = "exec ${rofi-powermenu}";
            "${modifier}+d" = "exec rofi -show drun -show-icons";
            "${modifier}+Shift+d" = "exec rofi -show run";
            "${modifier}+Tab" = "exec ${rofi-windows}";
            "${modifier}+Ctrl+e" =
              ''exec swaymsg "[app_id=emacs_scratch] scratchpad show, fullscreen enable" || exec emacs --name emacs_scratch'';
            "${modifier}+Ctrl+s" =
              ''exec swaymsg "[app_id=kitty_scratch] scratchpad show, fullscreen enable" || exec kitty --class kitty_scratch'';
          };
        in
        combined;
      keycodebindings =
        let
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
        in
        combined;
      startup = [
        {
          command = "cat /proc/acpi/button/lid/LID/state | awk '{print $2}' | grep -q 'closed' && swaymsg output eDP-1 disable";
          always = true;
        }
      ];
      window = lib.mkMerge [
        common.config.window
        {
          commands = [
            {
              criteria = {
                app_id = "emacs_scratch";
              };
              command = "move scratchpad, fullscreen enable, scratchpad show";
            }
            {
              criteria = {
                app_id = "kitty_scratch";
              };
              command = "move scratchpad, fullscreen enable, scratchpad show";
            }
          ];
        }
      ];
    };
    extraSessionCommands = # bash
      ''
        export MOZ_ENABLE_WAYLAND=1
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
    wrapperFeatures.gtk = true;
  };
}
