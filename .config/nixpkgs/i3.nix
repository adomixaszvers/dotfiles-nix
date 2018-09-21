{ pkgs, ... }:
{
    enable = true;
    config = let
      modifier = "Mod4";
      modeSystem = "System (l) lock, (e) logout, (s) suspend, (h) hibernate, (r) reboot, (Shift+s) shutdown";
      workspace1 = "1: ";
      workspace2  = "2: ";
      workspace3  = "3: ";
      workspace4  = "4: ";
      workspace5  = "5: ";
      workspace6  = "6: ";
      workspace10  = "10: ";
    in {
      inherit modifier;
      bars = [{
        statusCommand = "bumblebee-status -m title cpu memory layout pasink datetime -t gruvbox-powerline";
        fonts = [ "DejaVu Sans Mono, Icons 8" ];
      }];
      fonts = [ "DejaVuSansMono Nerd Font 8" ];
      gaps = {
        inner = 15;
        outer = 0;
        smartGaps = true;
        smartBorders = "on";
      };
      keybindings = {
        "${modifier}+Return" = "exec i3-sensible-terminal";
        "${modifier}+Shift+q" = "kill";
        "Shift+${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show run";
        "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";
        "${modifier}+Tab" = "exec ${pkgs.rofi}/bin/rofi -show window";

        "${modifier}+Left" = "focus left";
        "${modifier}+Down" = "focus down";
        "${modifier}+Up" = "focus up";
        "${modifier}+Right" = "focus right";

        "${modifier}+Shift+Left" = "move left";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Right" = "move right";

        "${modifier}+h" = "split h";
        "${modifier}+v" = "split v";
        "${modifier}+f" = "fullscreen toggle";

        "${modifier}+s" = "layout stacking";
        "${modifier}+w" = "layout tabbed";
        "${modifier}+e" = "layout toggle split";

        "${modifier}+Shift+space" = "floating toggle";
        "${modifier}+space" = "focus mode_toggle";
        "${modifier}+a" = "focus parent";
        "Shift+${modifier}+a" = "focus child";

        "${modifier}+Shift+c" = "reload";
        "${modifier}+Shift+r" = "restart";
        "${modifier}+Shift+e" = "exec i3-nagbar -t warning -m 'Do you want to exit i3?' -b 'Yes' 'i3-msg exit'";

        "${modifier}+r" = "mode resize";
        "${modifier}+Pause" = "mode \"${modeSystem}\"";
        "${modifier}+m" = "move workspace to output left";
      };
      keycodebindings = {
        "${modifier}+10" = "workspace ${workspace1}";
        "${modifier}+11" = "workspace ${workspace2}";
        "${modifier}+12" = "workspace ${workspace3}";
        "${modifier}+13" = "workspace ${workspace4}";
        "${modifier}+14" = "workspace ${workspace5}";
        "${modifier}+15" = "workspace ${workspace6}";
        "${modifier}+16" = "workspace 7";
        "${modifier}+17" = "workspace 8";
        "${modifier}+18" = "workspace 9";
        "${modifier}+19" = "workspace ${workspace10}";

        "Shift+${modifier}+10" = "move container to workspace ${workspace1}";
        "Shift+${modifier}+11" = "move container to workspace ${workspace2}";
        "Shift+${modifier}+12" = "move container to workspace ${workspace3}";
        "Shift+${modifier}+13" = "move container to workspace ${workspace4}";
        "Shift+${modifier}+14" = "move container to workspace ${workspace5}";
        "Shift+${modifier}+15" = "move container to workspace ${workspace6}";
        "Shift+${modifier}+16" = "move container to workspace 7";
        "Shift+${modifier}+17" = "move container to workspace 8";
        "Shift+${modifier}+18" = "move container to workspace 9";
        "Shift+${modifier}+19" = "move container to workspace ${workspace10}";
      };
      modes = {
        resize = {
          Down = "resize grow height 10 px or 10 ppt";
          Escape = "mode default";
          Left = "resize shrink width 10 px or 10 ppt";
          Return = "mode default";
          Right = "resize grow width 10 px or 10 ppt";
          Up = "resize shrink height 10 px or 10 ppt";
        };
        "${modeSystem}" = let locker = "i3lock && sleep 1"; in {
          l = "exec --no-startup-id ${locker}, mode default";
          e = "exec --no-startup-id i3-msg exit, mode default";
          s = "exec --no-startup-id ${locker} && systemctl suspend, mode default";
          h = "exec --no-startup-id ${locker} && systemctl hibernate, mode default";
          r = "exec --no-startup-id systemctl reboot, mode default";
          "Shift+s" = "exec --no-startup-id systemctl poweroff -i, mode default";

          # back to normal: Enter or Escape
          Return = "mode default";
          Escape = "mode default";
        };
      };
      startup = [
        { command = "feh --bg-scale ~/wallpaper.png"; notification = false; }
      ];
    };
  }
