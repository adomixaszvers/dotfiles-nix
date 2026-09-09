{
  config,
  pkgs,
  lib,
  myPkgs,
  ...
}:
{
  imports = [
    ../waybar
    ../dunst.nix
  ];
  home.packages = [
    # keep-sorted start
    pkgs.nautilus # for file chooser dialogs
    pkgs.wl-clipboard
    # keep-sorted end
  ];
  programs = {
    emacs.package = pkgs.emacs-pgtk;
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
  wayland.windowManager.niri = {
    enable = true;
    xwaylandSatellitePackage = pkgs.xwayland-satellite.overrideAttrs (old: {
      patches = old.patches ++ [
        (pkgs.fetchpatch {
          # https://github.com/Supreeeme/xwayland-satellite/pull/494
          name = "fix-popups";
          url = "https://github.com/Supreeeme/xwayland-satellite/commit/add2795134593faafce60e404a0a75df68e9ee0c.patch";
          hash = "sha256-/1zJYAIHC+xiVytHH5HDt83lZKLBGQQdAoS/y2ObTLc=";
        })
      ];
    });
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
        "Mod+Shift+Slash"."show-hotkey-overlay" = { };

        # Suggested binds for running programs: terminal, app launcher, screen locker.
        "Mod+Return" = {
          spawn = "kitty";
          _props.hotkey-overlay-title = "Open a Terminal: kitty";
        };
        "Mod+D" = {
          spawn = [
            "rofi"
            "-show-icons"
            "-combi-modi"
            "window,drun,run"
            "-show"
            "combi"
          ];
          _props.hotkey-overlay-title = "Run an Application: rofi";
        };
        "Mod+Shift+D" = {
          spawn = [
            "rofi"
            "-show"
            "run"
          ];
          _props.hotkey-overlay-title = "Run a Command: rofi";
        };
        "Super+Alt+L" = {
          spawn = "hyprlock";
          _props = {
            allow-when-locked = true;
            hotkey-overlay-title = "Lock the Screen: hyprlock";
          };
        };

        # You can also use a shell. Do this if you need pipes, multiple commands, etc.
        # Note: the entire command goes as a single argument in the end.
        # Mod+T { spawn "bash" "-c" "notify-send hello && exec alacritty"; }

        # Example volume keys mappings for PipeWire & WirePlumber.
        # The allow-when-locked=true property makes them work even when the session is locked.
        "XF86AudioRaiseVolume" = {
          spawn = [
            "wpctl"
            "set-volume"
            "@DEFAULT_AUDIO_SINK@"
            "0.1+"
          ];
          _props.allow-when-locked = true;
        };
        "XF86AudioLowerVolume" = {
          spawn = [
            "wpctl"
            "set-volume"
            "@DEFAULT_AUDIO_SINK@"
            "0.1-"
          ];
          _props.allow-when-locked = true;
        };
        "XF86AudioMute" = {
          spawn = [
            "wpctl"
            "set-mute"
            "@DEFAULT_AUDIO_SINK@"
            "toggle"
          ];
          _props.allow-when-locked = true;
        };
        "XF86AudioMicMute" = {
          spawn = [
            "wpctl"
            "set-mute"
            "@DEFAULT_AUDIO_SOURCE@"
            "toggle"
          ];
          _props.allow-when-locked = true;
        };
        "XF86AudioPlay" = {
          spawn = [
            "playerctl"
            "play-pause"
          ];
          _props.allow-when-locked = true;
        };
        "XF86AudioPrev" = {
          spawn = [
            "playerctl"
            "previous"
          ];
          _props.allow-when-locked = true;
        };
        "XF86AudioNext" = {
          spawn = [
            "playerctl"
            "next"
          ];
          _props.allow-when-locked = true;
        };

        # Open/close the Overview: a zoomed-out view of workspaces and windows.
        # You can also move the mouse into the top-left hot corner,
        # or do a four-finger swipe up on a touchpad.
        "Mod+O" = {
          toggle-overview = { };
          _props.repeat = false;
        };

        "Mod+Shift+Q".close-window = { };

        "Mod+Left".focus-column-left = { };
        "Mod+Down".focus-window-down = { };
        "Mod+Up".focus-window-up = { };
        "Mod+Right".focus-column-right = { };
        "Mod+H".focus-column-left = { };
        "Mod+J".focus-window-down = { };
        "Mod+K".focus-window-up = { };
        "Mod+L".focus-column-right = { };

        "Mod+Shift+Left".move-column-left = { };
        "Mod+Shift+Down".move-window-down = { };
        "Mod+Shift+Up".move-window-up = { };
        "Mod+Shift+Right".move-column-right = { };
        "Mod+Shift+H".move-column-left = { };
        "Mod+Shift+J".move-window-down = { };
        "Mod+Shift+K".move-window-up = { };
        "Mod+Shift+L".move-column-right = { };

        # Alternative commands that move across workspaces when reaching
        # the first or last window in a column.
        # Mod+J     { focus-window-or-workspace-down; }
        # Mod+K     { focus-window-or-workspace-up; }
        # Mod+Ctrl+J     { move-window-down-or-to-workspace-down; }
        # Mod+Ctrl+K     { move-window-up-or-to-workspace-up; }

        "Mod+Home".focus-column-first = { };
        "Mod+End".focus-column-last = { };
        "Mod+Ctrl+Home".move-column-to-first = { };
        "Mod+Ctrl+End".move-column-to-last = { };

        "Mod+Ctrl+Left".focus-monitor-left = { };
        "Mod+Ctrl+Down".focus-monitor-down = { };
        "Mod+Ctrl+Up".focus-monitor-up = { };
        "Mod+Ctrl+Right".focus-monitor-right = { };
        "Mod+Ctrl+H".focus-monitor-left = { };
        "Mod+Ctrl+J".focus-monitor-down = { };
        "Mod+Ctrl+K".focus-monitor-up = { };
        "Mod+Ctrl+L".focus-monitor-right = { };
        "Mod+Ctrl+O".spawn = lib.getExe myPkgs.niri-swap-monitors;

        "Mod+Shift+Ctrl+Left".move-column-to-monitor-left = { };
        "Mod+Shift+Ctrl+Down".move-column-to-monitor-down = { };
        "Mod+Shift+Ctrl+Up".move-column-to-monitor-up = { };
        "Mod+Shift+Ctrl+Right".move-column-to-monitor-right = { };
        "Mod+Shift+Ctrl+H".move-column-to-monitor-left = { };
        "Mod+Shift+Ctrl+J".move-column-to-monitor-down = { };
        "Mod+Shift+Ctrl+K".move-column-to-monitor-up = { };
        "Mod+Shift+Ctrl+L".move-column-to-monitor-right = { };

        # Alternatively, there are commands to move just a single window:
        # Mod+Shift+Ctrl+Left  { move-window-to-monitor-left; }
        # ...

        # And you can also move a whole workspace to another monitor:
        # Mod+Shift+Ctrl+Left  { move-workspace-to-monitor-left; }
        # ...
        "Mod+Ctrl+BracketLeft".move-workspace-to-monitor-previous = { };
        "Mod+Ctrl+BracketRight".move-workspace-to-monitor-next = { };

        "Mod+Page_Down".focus-workspace-down = { };
        "Mod+Page_Up".focus-workspace-up = { };
        "Mod+U".focus-workspace-down = { };
        "Mod+I".focus-workspace-up = { };
        "Mod+Shift+Page_Down".move-column-to-workspace-down = { };
        "Mod+Shift+Page_Up".move-column-to-workspace-up = { };
        "Mod+Shift+U".move-column-to-workspace-down = { };
        "Mod+Shift+I".move-column-to-workspace-up = { };

        # Alternatively, there are commands to move just a single window:
        # Mod+Ctrl+Page_Down { move-window-to-workspace-down; }
        # ...

        "Mod+Ctrl+Page_Down".move-workspace-down = { };
        "Mod+Ctrl+Page_Up".move-workspace-up = { };
        "Mod+Ctrl+U".move-workspace-down = { };
        "Mod+Ctrl+I".move-workspace-up = { };

        # You can bind mouse wheel scroll ticks using the following syntax.
        # These binds will change direction based on the natural-scroll setting.
        #
        # To avoid scrolling through workspaces really fast, you can use
        # the cooldown-ms property. The bind will be rate-limited to this value.
        # You can set a cooldown on any bind, but it's most useful for the wheel.
        "Mod+WheelScrollDown" = {
          focus-workspace-down = { };
          _props.cooldown-ms = 150;
        };
        "Mod+WheelScrollUp" = {
          focus-workspace-up = { };
          _props.cooldown-ms = 150;
        };
        "Mod+Ctrl+WheelScrollDown" = {
          move-column-to-workspace-down = { };
          _props.cooldown-ms = 150;
        };
        "Mod+Ctrl+WheelScrollUp" = {
          move-column-to-workspace-up = { };
          _props.cooldown-ms = 150;
        };

        "Mod+WheelScrollRight".focus-column-right = { };
        "Mod+WheelScrollLeft".focus-column-left = { };
        "Mod+Ctrl+WheelScrollRight".move-column-right = { };
        "Mod+Ctrl+WheelScrollLeft".move-column-left = { };

        # Usually scrolling up and down with Shift in applications results in
        # horizontal scrolling; these binds replicate that.
        "Mod+Shift+WheelScrollDown".focus-column-right = { };
        "Mod+Shift+WheelScrollUp".focus-column-left = { };
        "Mod+Ctrl+Shift+WheelScrollDown".move-column-right = { };
        "Mod+Ctrl+Shift+WheelScrollUp".move-column-left = { };

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
        "Mod+1".focus-workspace = 1;
        "Mod+2".focus-workspace = 2;
        "Mod+3".focus-workspace = 3;
        "Mod+4".focus-workspace = 4;
        "Mod+5".focus-workspace = 5;
        "Mod+6".focus-workspace = 6;
        "Mod+7".focus-workspace = 7;
        "Mod+8".focus-workspace = 8;
        "Mod+9".focus-workspace = 9;
        "Mod+Shift+1".move-column-to-workspace = 1;
        "Mod+Shift+2".move-column-to-workspace = 2;
        "Mod+Shift+3".move-column-to-workspace = 3;
        "Mod+Shift+4".move-column-to-workspace = 4;
        "Mod+Shift+5".move-column-to-workspace = 5;
        "Mod+Shift+6".move-column-to-workspace = 6;
        "Mod+Shift+7".move-column-to-workspace = 7;
        "Mod+Shift+8".move-column-to-workspace = 8;
        "Mod+Shift+9".move-column-to-workspace = 9;

        # Alternatively, there are commands to move just a single window:
        # Mod+Ctrl+1 { move-window-to-workspace 1; }

        # Switches focus between the current and the previous workspace.
        # Mod+Tab { focus-workspace-previous; }

        # The following binds move the focused window in and out of a column.
        # If the window is alone, they will consume it into the nearby column to the side.
        # If the window is already in a column, they will expel it out.
        "Mod+BracketLeft".consume-or-expel-window-left = { };
        "Mod+BracketRight".consume-or-expel-window-right = { };

        # Consume one window from the right to the bottom of the focused column.
        "Mod+Comma".consume-window-into-column = { };
        # Expel the bottom window from the focused column to the right.
        "Mod+Period".expel-window-from-column = { };

        "Mod+R".switch-preset-column-width = { };
        "Mod+Shift+R".switch-preset-window-height = { };
        "Mod+Ctrl+R".reset-window-height = { };
        "Mod+F".maximize-column = { };
        "Mod+Shift+F".fullscreen-window = { };
        "Mod+Ctrl+F".expand-column-to-available-width = { };
        "Mod+N".maximize-window-to-edges = { };

        "Mod+C".center-column = { };

        # Center all fully visible columns on screen.
        "Mod+Ctrl+C".center-visible-columns = { };

        # Finer width adjustments.
        # This command can also:
        # * set width in pixels: "1000"
        # * adjust width in pixels: "-5" or "+5"
        # * set width as a percentage of screen width: "25%"
        # * adjust width as a percentage of screen width: "-10%" or "+10%"
        # Pixel sizes use logical, or scaled, pixels. I.e. on an output with scale 2.0,
        # set-column-width "100" will make the column occupy 200 physical screen pixels.
        "Mod+Minus".set-column-width = "-10%";
        "Mod+Equal".set-column-width = "+10%";

        # Finer height adjustments when in column with other windows.
        "Mod+Shift+Minus".set-window-height = "-10%";
        "Mod+Shift+Equal".set-window-height = "+10%";

        # Move the focused window between the floating and the tiling layout.
        "Mod+V".toggle-window-floating = { };
        "Mod+Shift+V".switch-focus-between-floating-and-tiling = { };

        # Toggle tabbed column display mode.
        # Windows in this column will appear as vertical tabs,
        # rather than stacked on top of each other.
        "Mod+W".toggle-column-tabbed-display = { };

        # Actions to switch layouts.
        # Note: if you uncomment these, make sure you do NOT have
        # a matching layout switch hotkey configured in xkb options above.
        # Having both at once on the same hotkey will break the switching,
        # since it will switch twice upon pressing the hotkey (once by xkb, once by niri).
        # Mod+Space       { switch-layout "next"; }
        # Mod+Shift+Space { switch-layout "prev"; }

        "Print".screenshot = { };
        "Ctrl+Print".screenshot-screen = { };
        "Alt+Print".screenshot-window = { };

        # Applications such as remote-desktop clients and software KVM switches may
        # request that niri stops processing the keyboard shortcuts defined here
        # so they may, for example, forward the key presses as-is to a remote machine.
        # It's a good idea to bind an escape hatch to toggle the inhibitor,
        # so a buggy application can't hold your session hostage.
        #
        # The allow-inhibiting=false property can be applied to other binds as well,
        # which ensures niri always processes them, even when an inhibitor is active.
        "Mod+Escape" = {
          toggle-keyboard-shortcuts-inhibit = { };
          _props.allow-inhibiting = false;
        };

        # The quit action will show a confirmation dialog to avoid accidental exits.
        "Mod+Shift+C".quit = { };
        "Ctrl+Alt+Delete".quit = { };

        # Powers off the monitors. To turn them back on, do any input like
        # moving the mouse or pressing any other key.
        "Mod+Shift+P".power-off-monitors = { };

        "Mod+F4".spawn = "rofi-powermenu";
        "Mod+F5".spawn = "playerctl play-pause";
        "Mod+F6".spawn = "playerctl previous";
        "Mod+F7".spawn = "playerctl next";
      };
      prefer-no-csd = true;
      spawn-at-startup._args = [
        "systemctl"
        "--user"
        "reset-failed"
      ];
      input = {
        focus-follows-mouse = {
          _props = {
            max-scroll-amount = "10%";
          };
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
        focus-ring.off = { };
        border =
          let
            inherit (config.lib.stylix.colors.withHashtag) base0D base03;
          in
          {
            active-color = base0D;
            inactive-color = base03;
          };

      };
      cursor = {
        xcursor-size = config.stylix.cursor.size;
        xcursor-theme = config.stylix.cursor.name;
      };
      environment = {
        QT_QPA_PLATFORM = "wayland";
        KITTY_CONF_FONT = "font_size 9.0";
        _JAVA_AWT_WM_NONREPARENTING = "1";
      };
      _children = [
        {
          window-rule._children = [
            {
              match._props = {
                app-id = "firefox$";
                title = "^Picture-in-Picture$";
              };
            }
            {
              open-floating = true;
            }
          ];
        }
        {
          window-rule._children = [
            {
              match._props = {
                app-id = "firefox$";
              };
            }
            {
              open-maximized = true;
            }
            {
              open-on-workspace = "browser";
            }
          ];
        }
        {
          window-rule._children = [
            {
              match._props = {
                app-id = "^jetbrains-idea$";
              };
            }
            {
              open-on-workspace = "dev";
            }
          ];
        }
        {
          window-rule._children = [
            {
              match._props = {
                app-id = "^steam$";
              };
              open-on-workspace = "game";
            }
          ];
        }
      ];
    };
  };
  stylix.targets = {
    hyprlock.enable = true;
    hyprpaper.enable = true;
    waybar = {
      enable = true;
      enableCenterBackColors = true;
      enableLeftBackColors = true;
      enableRightBackColors = false;
    };
  };
}
