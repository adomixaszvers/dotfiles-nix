{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    ../waybar
    ../dunst.nix
  ];
  home = {
    packages = with pkgs; [
      grimblast
      pamixer
      hyprpicker
      wl-clipboard
      wdisplays
      xwayland
    ];
    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };
  };
  programs = {
    emacs.package = pkgs.emacs-pgtk;
    rofi = {
      extraConfig.modi = "window,drun,run,ssh";
    };
    waybar = {
      settings.mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [ "hyprland/workspaces" ];
        modules-center = [ "hyprland/window" ];
        modules-right = (lib.optional config.gui.hasBattery "battery") ++ [
          "hyprland/language"
          "pulseaudio"
          "cpu"
          "memory"
          "temperature"
          "clock"
          "tray"
        ];
        "hyprland/language" = {
          format-lt = "lt";
          format-en = "us";
        };
        "hyprland/window" = {
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
      systemd = {
        enable = true;
        target = "hyprland-session.target";
      };
    };
  };
  services = {
    hyprpaper.enable = true;
    swayidle = {
      timeouts =
        let
          hyprctl = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";
        in
        [
          {
            timeout = 360;
            command = "${hyprctl} dispatch dpms off";
            resumeCommand = "${hyprctl} dispatch dpms on";
          }
        ];
    };
  };
  stylix.targets = {
    hyprland.enable = true;
    hyprlock.enable = true;
    hyprpaper.enable = true;
    waybar = {
      enable = true;
      enableCenterBackColors = true;
      enableLeftBackColors = true;
      enableRightBackColors = false;
    };
  };
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;
    settings = {

      # This is an example Hyprland config file.
      #
      # Refer to the wiki for more information.

      #
      # Please note not all available settings / options are set here.
      # For a full list, see the wiki
      #

      # See https://wiki.hyprland.org/Configuring/Monitors/
      monitor = [ ",preferred,auto,1" ];

      # See https://wiki.hyprland.org/Configuring/Keywords/ for more

      # Execute your favorite apps at launch
      # exec-once = waybar & hyprpaper & firefox

      # Source a file (multi-file configs)
      # source = ~/.config/hypr/myColors.conf

      # Some default env vars.
      env = [
        "KITTY_CONF_FONT,font_size 9.0"
      ];
      # sets xwayland scale

      # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
      input = {
        kb_layout = "lt,us";
        kb_options = "grp:caps_toggle";
        numlock_by_default = true;

        follow_mouse = 1;

        touchpad = {
          natural_scroll = false;
        };

        sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
      };

      xwayland = {
        force_zero_scaling = true;
      };

      general = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more

        gaps_in = 5;
        gaps_out = 20;
        border_size = 2;

        layout = "master";
        allow_tearing = true;
      };

      decoration = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more

        rounding = 5;
        blur = {
          enabled = lib.mkDefault true;
          size = 3;
          passes = 1;
          new_optimizations = true;
        };

        shadow = {
          enabled = lib.mkDefault true;
          range = 4;
          render_power = 3;
        };

      };

      dwindle = {
        # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
        pseudotile = true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
        preserve_split = true; # you probably want this
      };

      master = {
        # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
        new_status = "slave";
      };

      gestures = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        gesture = "3, horizontal, workspace";
      };

      misc = {
        vrr = 1;
        disable_hyprland_logo = true; # no anime
        new_window_takes_over_fullscreen = 1; # take over
        exit_window_retains_fullscreen = true;
      };

      # Example windowrule v1
      # windowrule = float, ^(kitty)$
      # Example windowrule v2
      # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

      # See https://wiki.hyprland.org/Configuring/Keywords/ for more
      "$mainMod" = "SUPER";
      "$showVolume" =
        ''dunstify -i audio-card -t 2000 -h string:x-dunst-stack-tag:volume "Volume $(pamixer --get-volume)"'';

      # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
      bind = [
        "$mainMod, Return, exec, kitty"
        "$mainMod SHIFT, Q, killactive,"
        "$mainMod SHIFT, C, exit,"
        "$mainMod, T, togglefloating,"
        "$mainMod, D, exec, rofi -show-icons -combi-modi window,drun,run -show combi"
        "$mainMod SHIFT, D, exec, rofi -show run"
        "$mainMod, F, fullscreen, 1"
        "$mainMod SHIFT, F, fullscreen, 0" # true fullscreen
        "$mainMod, P, pseudo," # dwindle
        "$mainMod, S, togglesplit," # dwindle
        "$mainMod, F4, exec, rofi-powermenu"

        "$mainMod, W, focusmonitor, 0"
        "$mainMod, E, focusmonitor, 1"
        "$mainMod, R, focusmonitor, 2"

        ", Print, exec, grimblast copy output"
        "ALT, Print, exec, grimblast copy area"
        "$mainMod, SLASH, exec, hyprpicker -a"

        # Move focus with mainMod + arrow keys
        "$mainMod, left, movefocus, l"
        "$mainMod, right, movefocus, r"
        "$mainMod, up, movefocus, u"
        "$mainMod, down, movefocus, d"

        "$mainMod, J, layoutmsg, cyclenext"
        "$mainMod, K, layoutmsg, cycleprev"
        "$mainMod SHIFT, J, layoutmsg, swapnext"
        "$mainMod SHIFT, K, layoutmsg, swapprev"
        "$mainMod SHIFT, Return, layoutmsg, swapwithmaster"
        "$mainMod, SPACE, layoutmsg, orientationcycle left top"

        "$mainMod, C, cyclenext, tiled"

        "$mainMod, bracketleft, focusmonitor, -1"
        "$mainMod, bracketright, focusmonitor, +1"

        "$mainMod CTRL, bracketleft, swapactiveworkspaces, current -1"
        "$mainMod CTRL, bracketright, swapactiveworkspaces, current +1"

        # Scroll through existing workspaces with mainMod + scroll
        "$mainMod, mouse_down, workspace, e+1"
        "$mainMod, mouse_up, workspace, e-1"

        "$mainMod, minus, exec, pamixer -d 5 && $showVolume"
        "$mainMod, equal, exec, pamixer -i 5 && $showVolume"
        "$mainMod, zcaron, exec, pamixer -i 5 && $showVolume"
        "$mainMod, F5, exec, playerctl play-pause"
        "$mainMod, F6, exec, playerctl previous"
        "$mainMod, F7, exec, playerctl next"
      ]
      ++ (builtins.concatMap (
        x:
        let
          ws = toString x;
          keyCode = toString (x + 9);
        in
        [
          "$mainMod, ${keyCode}, focusworkspaceoncurrentmonitor, ${ws}"
          "$mainMod SHIFT, ${keyCode}, movetoworkspace, ${ws}"
        ]
      ) (builtins.genList (x: x + 1) 10));

      # Move/resize windows with mainMod + LMB/RMB and dragging
      bindm = [
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];

      bindl = [
        ", XF86AudioLowerVolume, exec, pamixer -d 5 && $showVolume"
        ", XF86AudioRaiseVolume, exec, pamixer -i 5 && $showVolume"
        ", XF86AudioMute, exec, pamixer -t && $showVolume"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
        ", XF86AudioNext, exec, playerctl next"
      ];

      windowrulev2 = [
        "float,class:^(Vampire_Survivors)$"
        "workspace 1 silent,class:^(Google-chrome)$"
        "workspace 1 silent,class:^(firefox)$"
        "workspace 3 silent,class:^(jetbrains-idea)$,floating:0"
        "workspace 5 silent,class:^(steam)$"
        "workspace 7,class:^(steam_app_3191030)$"
        "workspace 9 silent,class:^(org.keepassxc.KeePassXC)$,floating:0"
        "tile,class:^(com-eviware-soapui-SoapUI)$,title:^(SoapUI)(.*)$"
        # fix steam menus
        # "stayfocused, title:^()$,class:^(steam)$"
        # "minsize 1 1, title:^()$,class:^(steam)$"
        "noanim,floating:1"
        "fullscreen,class:^(.gamescope-wrapped)$"

        # smart gaps
        # see https://wiki.hyprland.org/0.45.0/Configuring/Workspace-Rules/#smart-gaps
        # damn you, Vaxry
        # no_gaps_when_only was good enough :/
        "bordersize 0, floating:0, onworkspace:w[t1]"
        "rounding 0, floating:0, onworkspace:w[t1]"
        "bordersize 0, floating:0, onworkspace:w[tg1]"
        "rounding 0, floating:0, onworkspace:w[tg1]"
        "bordersize 0, floating:0, onworkspace:f[1]"
        "rounding 0, floating:0, onworkspace:f[1]"
      ];

      # for smart gaps
      workspace = [
        "w[t1], gapsout:0, gapsin:0"
        "w[tg1], gapsout:0, gapsin:0"
        "f[1], gapsout:0, gapsin:0"
      ];
    };
  };
}
