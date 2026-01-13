{
  config,
  inputs,
  pkgs,
  lib,
  myPkgs,
  ...
}:
{
  imports = [
    inputs.niri.homeModules.niri
    inputs.niri.homeModules.stylix
    ../waybar
    ../dunst.nix
  ];
  home.packages = [
    pkgs.wl-clipboard
    pkgs.xwayland-satellite
  ];
  programs = {
    emacs.package = pkgs.emacs-pgtk;
    niri = {
      enable = true;
      settings = {
        binds = {

          # Keys consist of modifiers separated by + signs, followed by an XKB key name
          # in the end. To find an XKB name for a particular key, you may use a program
          # like wev.
          #
          # "Mod" is a special modifier equal to Super when running on a TTY, and to Alt
          # when running as a winit window.
          #
          # Most actions that you can bind here can also be invoked programmatically with
          # `niri msg action do-something`.

          # Mod-Shift-/, which is usually the same as Mod-?,
          # shows a list of important hotkeys.
          "Mod+Shift+Slash".action."show-hotkey-overlay" = [ ];

          # Suggested binds for running programs: terminal, app launcher, screen locker.
          "Mod+Return" = {
            action.spawn = "kitty";
            hotkey-overlay.title = "Open a Terminal: kitty";
          };
          "Mod+D" = {
            action.spawn = [
              "rofi"
              "-show-icons"
              "-combi-modi"
              "window,drun,run"
              "-show"
              "combi"
            ];
            hotkey-overlay.title = "Run an Application: rofi";
          };
          "Mod+Shift+D" = {
            action.spawn = [
              "rofi"
              "-show"
              "run"
            ];
            hotkey-overlay.title = "Run a Command: rofi";
          };
          "Super+Alt+L" = {
            action.spawn = "hyprlock";
            hotkey-overlay.title = "Lock the Screen: hyprlock";
          };

          # You can also use a shell. Do this if you need pipes, multiple commands, etc.
          # Note: the entire command goes as a single argument in the end.
          # Mod+T { spawn "bash" "-c" "notify-send hello && exec alacritty"; }

          # Example volume keys mappings for PipeWire & WirePlumber.
          # The allow-when-locked=true property makes them work even when the session is locked.
          "XF86AudioRaiseVolume" = {
            action.spawn = [
              "wpctl"
              "set-volume"
              "@DEFAULT_AUDIO_SINK@"
              "0.1+"
            ];
            allow-when-locked = true;
          };
          "XF86AudioLowerVolume" = {
            action.spawn = [
              "wpctl"
              "set-volume"
              "@DEFAULT_AUDIO_SINK@"
              "0.1-"
            ];
            allow-when-locked = true;
          };
          "XF86AudioMute" = {
            action.spawn = [
              "wpctl"
              "set-mute"
              "@DEFAULT_AUDIO_SINK@"
              "toggle"
            ];
            allow-when-locked = true;
          };
          "XF86AudioMicMute" = {
            action.spawn = [
              "wpctl"
              "set-mute"
              "@DEFAULT_AUDIO_SOURCE@"
              "toggle"
            ];
            allow-when-locked = true;
          };

          # Open/close the Overview: a zoomed-out view of workspaces and windows.
          # You can also move the mouse into the top-left hot corner,
          # or do a four-finger swipe up on a touchpad.
          "Mod+O" = {
            action.toggle-overview = [ ];
            repeat = false;
          };

          "Mod+Shift+Q".action.close-window = [ ];

          "Mod+Left".action.focus-column-left = [ ];
          "Mod+Down".action.focus-window-down = [ ];
          "Mod+Up".action.focus-window-up = [ ];
          "Mod+Right".action.focus-column-right = [ ];
          "Mod+H".action.focus-column-left = [ ];
          "Mod+J".action.focus-window-down = [ ];
          "Mod+K".action.focus-window-up = [ ];
          "Mod+L".action.focus-column-right = [ ];

          "Mod+Shift+Left".action.move-column-left = [ ];
          "Mod+Shift+Down".action.move-window-down = [ ];
          "Mod+Shift+Up".action.move-window-up = [ ];
          "Mod+Shift+Right".action.move-column-right = [ ];
          "Mod+Shift+H".action.move-column-left = [ ];
          "Mod+Shift+J".action.move-window-down = [ ];
          "Mod+Shift+K".action.move-window-up = [ ];
          "Mod+Shift+L".action.move-column-right = [ ];

          # Alternative commands that move across workspaces when reaching
          # the first or last window in a column.
          # Mod+J     { focus-window-or-workspace-down; }
          # Mod+K     { focus-window-or-workspace-up; }
          # Mod+Ctrl+J     { move-window-down-or-to-workspace-down; }
          # Mod+Ctrl+K     { move-window-up-or-to-workspace-up; }

          "Mod+Home".action.focus-column-first = [ ];
          "Mod+End".action.focus-column-last = [ ];
          "Mod+Ctrl+Home".action.move-column-to-first = [ ];
          "Mod+Ctrl+End".action.move-column-to-last = [ ];

          "Mod+Ctrl+Left".action.focus-monitor-left = [ ];
          "Mod+Ctrl+Down".action.focus-monitor-down = [ ];
          "Mod+Ctrl+Up".action.focus-monitor-up = [ ];
          "Mod+Ctrl+Right".action.focus-monitor-right = [ ];
          "Mod+Ctrl+H".action.focus-monitor-left = [ ];
          "Mod+Ctrl+J".action.focus-monitor-down = [ ];
          "Mod+Ctrl+K".action.focus-monitor-up = [ ];
          "Mod+Ctrl+L".action.focus-monitor-right = [ ];

          "Mod+Shift+Ctrl+Left".action.move-column-to-monitor-left = [ ];
          "Mod+Shift+Ctrl+Down".action.move-column-to-monitor-down = [ ];
          "Mod+Shift+Ctrl+Up".action.move-column-to-monitor-up = [ ];
          "Mod+Shift+Ctrl+Right".action.move-column-to-monitor-right = [ ];
          "Mod+Shift+Ctrl+H".action.move-column-to-monitor-left = [ ];
          "Mod+Shift+Ctrl+J".action.move-column-to-monitor-down = [ ];
          "Mod+Shift+Ctrl+K".action.move-column-to-monitor-up = [ ];
          "Mod+Shift+Ctrl+L".action.move-column-to-monitor-right = [ ];

          # Alternatively, there are commands to move just a single window:
          # Mod+Shift+Ctrl+Left  { move-window-to-monitor-left; }
          # ...

          # And you can also move a whole workspace to another monitor:
          # Mod+Shift+Ctrl+Left  { move-workspace-to-monitor-left; }
          # ...
          "Mod+Ctrl+BracketLeft".action.move-workspace-to-monitor-previous = [ ];
          "Mod+Ctrl+BracketRight".action.move-workspace-to-monitor-next = [ ];
          "Mod+Ctrl+O".action.spawn = lib.getExe myPkgs.niri-swap-monitors;

          "Mod+Page_Down".action.focus-workspace-down = [ ];
          "Mod+Page_Up".action.focus-workspace-up = [ ];
          "Mod+U".action.focus-workspace-down = [ ];
          "Mod+I".action.focus-workspace-up = [ ];
          "Mod+Shift+Page_Down".action.move-column-to-workspace-down = [ ];
          "Mod+Shift+Page_Up".action.move-column-to-workspace-up = [ ];
          "Mod+Shift+U".action.move-column-to-workspace-down = [ ];
          "Mod+Shift+I".action.move-column-to-workspace-up = [ ];

          # Alternatively, there are commands to move just a single window:
          # Mod+Ctrl+Page_Down { move-window-to-workspace-down; }
          # ...

          "Mod+Ctrl+Page_Down".action.move-workspace-down = [ ];
          "Mod+Ctrl+Page_Up".action.move-workspace-up = [ ];
          "Mod+Ctrl+U".action.move-workspace-down = [ ];
          "Mod+Ctrl+I".action.move-workspace-up = [ ];

          # You can bind mouse wheel scroll ticks using the following syntax.
          # These binds will change direction based on the natural-scroll setting.
          #
          # To avoid scrolling through workspaces really fast, you can use
          # the cooldown-ms property. The bind will be rate-limited to this value.
          # You can set a cooldown on any bind, but it's most useful for the wheel.
          "Mod+WheelScrollDown" = {
            action.focus-workspace-down = [ ];
            cooldown-ms = 150;
          };
          "Mod+WheelScrollUp" = {
            action.focus-workspace-up = [ ];
            cooldown-ms = 150;
          };
          "Mod+Ctrl+WheelScrollDown" = {
            action.move-column-to-workspace-down = [ ];
            cooldown-ms = 150;
          };
          "Mod+Ctrl+WheelScrollUp" = {
            action.move-column-to-workspace-up = [ ];
            cooldown-ms = 150;
          };

          "Mod+WheelScrollRight".action.focus-column-right = [ ];
          "Mod+WheelScrollLeft".action.focus-column-left = [ ];
          "Mod+Ctrl+WheelScrollRight".action.move-column-right = [ ];
          "Mod+Ctrl+WheelScrollLeft".action.move-column-left = [ ];

          # Usually scrolling up and down with Shift in applications results in
          # horizontal scrolling; these binds replicate that.
          "Mod+Shift+WheelScrollDown".action.focus-column-right = [ ];
          "Mod+Shift+WheelScrollUp".action.focus-column-left = [ ];
          "Mod+Ctrl+Shift+WheelScrollDown".action.move-column-right = [ ];
          "Mod+Ctrl+Shift+WheelScrollUp".action.move-column-left = [ ];

          # Similarly, you can bind touchpad scroll "ticks".
          # Touchpad scrolling is continuous, so for these binds it is split into
          # discrete intervals.
          # These binds are also affected by touchpad's natural-scroll, so these
          # example binds are "inverted", since we have natural-scroll enabled for
          # touchpads by default.
          # Mod+TouchpadScrollDown { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02+"; }
          # Mod+TouchpadScrollUp   { spawn "wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.02-"; }

          # You can refer to workspaces by index. However, keep in mind that
          # niri is a dynamic workspace system, so these commands are kind of
          # "best effort". Trying to refer to a workspace index bigger than
          # the current workspace count will instead refer to the bottommost
          # (empty) workspace.
          #
          # For example, with 2 workspaces + 1 empty, indices 3, 4, 5 and so on
          # will all refer to the 3rd workspace.
          "Mod+1".action.focus-workspace = 1;
          "Mod+2".action.focus-workspace = 2;
          "Mod+3".action.focus-workspace = 3;
          "Mod+4".action.focus-workspace = 4;
          "Mod+5".action.focus-workspace = 5;
          "Mod+6".action.focus-workspace = 6;
          "Mod+7".action.focus-workspace = 7;
          "Mod+8".action.focus-workspace = 8;
          "Mod+9".action.focus-workspace = 9;
          "Mod+Shift+1".action.move-column-to-workspace = 1;
          "Mod+Shift+2".action.move-column-to-workspace = 2;
          "Mod+Shift+3".action.move-column-to-workspace = 3;
          "Mod+Shift+4".action.move-column-to-workspace = 4;
          "Mod+Shift+5".action.move-column-to-workspace = 5;
          "Mod+Shift+6".action.move-column-to-workspace = 6;
          "Mod+Shift+7".action.move-column-to-workspace = 7;
          "Mod+Shift+8".action.move-column-to-workspace = 8;
          "Mod+Shift+9".action.move-column-to-workspace = 9;

          # Alternatively, there are commands to move just a single window:
          # Mod+Ctrl+1 { move-window-to-workspace 1; }

          # Switches focus between the current and the previous workspace.
          # Mod+Tab { focus-workspace-previous; }

          # The following binds move the focused window in and out of a column.
          # If the window is alone, they will consume it into the nearby column to the side.
          # If the window is already in a column, they will expel it out.
          "Mod+BracketLeft".action.consume-or-expel-window-left = [ ];
          "Mod+BracketRight".action.consume-or-expel-window-right = [ ];

          # Consume one window from the right to the bottom of the focused column.
          "Mod+Comma".action.consume-window-into-column = [ ];
          # Expel the bottom window from the focused column to the right.
          "Mod+Period".action.expel-window-from-column = [ ];

          "Mod+R".action.switch-preset-column-width = [ ];
          "Mod+Shift+R".action.switch-preset-window-height = [ ];
          "Mod+Ctrl+R".action.reset-window-height = [ ];
          "Mod+F".action.maximize-column = [ ];
          "Mod+Shift+F".action.fullscreen-window = [ ];
          "Mod+Ctrl+F".action.expand-column-to-available-width = [ ];
          "Mod+N".action.maximize-window-to-edges = [ ];

          "Mod+C".action.center-column = [ ];

          # Center all fully visible columns on screen.
          "Mod+Ctrl+C".action.center-visible-columns = [ ];

          # Finer width adjustments.
          # This command can also:
          # * set width in pixels: "1000"
          # * adjust width in pixels: "-5" or "+5"
          # * set width as a percentage of screen width: "25%"
          # * adjust width as a percentage of screen width: "-10%" or "+10%"
          # Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
          # set-column-width "100" will make the column occupy 200 physical screen pixels.
          "Mod+Minus".action.set-column-width = "-10%";
          "Mod+Equal".action.set-column-width = "+10%";

          # Finer height adjustments when in column with other windows.
          "Mod+Shift+Minus".action.set-window-height = "-10%";
          "Mod+Shift+Equal".action.set-window-height = "+10%";

          # Move the focused window between the floating and the tiling layout.
          "Mod+V".action.toggle-window-floating = [ ];
          "Mod+Shift+V".action.switch-focus-between-floating-and-tiling = [ ];

          # Toggle tabbed column display mode.
          # Windows in this column will appear as vertical tabs,
          # rather than stacked on top of each other.
          "Mod+W".action.toggle-column-tabbed-display = [ ];

          # Actions to switch layouts.
          # Note: if you uncomment these, make sure you do NOT have
          # a matching layout switch hotkey configured in xkb options above.
          # Having both at once on the same hotkey will break the switching,
          # since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
          # Mod+Space       { switch-layout "next"; }
          # Mod+Shift+Space { switch-layout "prev"; }

          "Print".action.screenshot = [ ];
          "Ctrl+Print".action.screenshot-screen = [ ];
          "Alt+Print".action.screenshot-window = [ ];

          # Applications such as remote-desktop clients and software KVM switches may
          # request that niri stops processing the keyboard shortcuts defined here
          # so they may, for example, forward the key presses as-is to a remote machine.
          # It's a good idea to bind an escape hatch to toggle the inhibitor,
          # so a buggy application can't hold your session hostage.
          #
          # The allow-inhibiting=false property can be applied to other binds as well,
          # which ensures niri always processes them, even when an inhibitor is active.
          "Mod+Escape" = {
            action.toggle-keyboard-shortcuts-inhibit = [ ];
            allow-inhibiting = false;
          };

          # The quit action will show a confirmation dialog to avoid accidental exits.
          "Mod+Shift+C".action.quit = [ ];
          "Ctrl+Alt+Delete".action.quit = [ ];

          # Powers off the monitors. To turn them back on, do any input like
          # moving the mouse or pressing any other key.
          "Mod+Shift+P".action.power-off-monitors = [ ];

          "Mod+F4".action.spawn = "rofi-powermenu";
          "Mod+F5".action.spawn = "playerctl play-pause";
          "Mod+F6".action.spawn = "playerctl previous";
          "Mod+F7".action.spawn = "playerctl next";
        };
        prefer-no-csd = true;
        spawn-at-startup = [
          {
            argv = [
              "systemctl"
              "--user"
              "reset-failed"
            ];
          }
        ];
        input = {
          focus-follows-mouse = {
            enable = true;
            max-scroll-amount = "10%";
          };
          keyboard = {
            numlock = true;
            xkb = {
              layout = "lt,us";
              options = "grp:caps_toggle";
            };
          };
        };
        layout = {
          gaps = 8;
          default-column-width.proportion = 0.5;
        };
        environment = {
          QT_QPA_PLATFORM = "wayland";
          KITTY_CONF_FONT = "font_size 9.0";
          _JAVA_AWT_WM_NONREPARENTING = "1";
        };
        window-rules = [
          {
            matches = [
              {
                app-id = "firefox$";
                title = "^Picture-in-Picture$";
              }
            ];
            open-floating = true;
          }
          {
            matches = [
              { app-id = "firefox$"; }
            ];
            open-maximized = true;
            open-on-workspace = "browser";
          }
          {
            matches = [ { app-id = "^jetbrains-idea$"; } ];
            open-on-workspace = "dev";
          }
          {
            matches = [
              { app-id = "^steam$"; }
            ];
            open-on-workspace = "game";
          }
        ];
      };
      package = pkgs.niri;
    };
    rofi = {
      extraConfig.modi = "drun,run,window,combi";
    };
    waybar = {
      systemd.enable = true;
      settings.mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [ "niri/workspaces" ];
        modules-center = [ "niri/window" ];
        modules-right = (lib.optional config.gui.hasBattery "battery") ++ [
          "niri/language"
          "pulseaudio"
          "cpu"
          "memory"
          "temperature"
          "clock"
          "tray"
        ];
        "niri/language" = {
          format-lt = "lt";
          format-en = "us";
        };
        "niri/window" = {
          # format = "{title:.100}";
          separate-outputs = true;
        };
        temperature.thermal-zone = config.gui.thermal-zone;
      };
      style = # css
        ''
          window#waybar.fullscreen #window {
            border-radius: 8px;
          }

          /* see https://github.com/Alexays/Waybar/issues/2793#issuecomment-2039369688 */
          #language {
            min-width: 20px;
          }
        '';
    };
  };
  stylix.targets = {
    hyprlock.enable = true;
    hyprpaper.enable = true;
    niri.enable = true;
    waybar = {
      enable = true;
      enableCenterBackColors = true;
      enableLeftBackColors = true;
      enableRightBackColors = false;
    };
  };
}
