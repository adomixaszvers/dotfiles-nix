{
  pkgs,
  myPkgs,
  config,
  ...
}:
{
  imports = [
    ../waybar
    ../dunst.nix
  ];
  home.packages = with pkgs; [
    grimblast
    pamixer
    swayidle
    wl-clipboard
    wdisplays
    xwayland
  ];
  programs = {
    emacs.package = pkgs.emacs29-pgtk;
    rofi = {
      package = pkgs.rofi-wayland;
      extraConfig.modi = "drun,run,ssh";
    };
    waybar = {
      settings.mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [ "hyprland/workspaces" ];
        modules-center = [ "hyprland/window" ];
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
          scroll-step = 5.0;
        };
        temperature.thermal-zone = config.gui.thermal-zone;
      };
      systemd = {
        enable = true;
        target = "hyprland-session.target";
      };
    };
  };
  services.swayidle = {
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
  wayland.windowManager.hyprland = {
    enable = true;
    package = pkgs.hyprland.overrideAttrs (_old: {
      patches = [ (builtins.path { path = ./hyprctl-deps.patch; }) ];
    });
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
        "XCURSOR_SIZE,24"
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
        "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
        "col.inactive_border" = "rgba(595959aa)";

        layout = "master";
      };

      decoration = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more

        rounding = 5;
        blur = {
          enabled = true;
          size = 3;
          passes = 1;
          new_optimizations = true;
        };

        drop_shadow = true;
        shadow_range = 4;
        shadow_render_power = 3;
        "col.shadow" = "rgba(1a1a1aee)";
      };

      animations = {
        enabled = false;
      };

      dwindle = {
        # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
        pseudotile = true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
        preserve_split = true; # you probably want this
        no_gaps_when_only = true;
      };

      master = {
        # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
        new_status = "master";
        no_gaps_when_only = true;
      };

      gestures = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        workspace_swipe = false;
      };

      misc = {
        vrr = 1;
        disable_hyprland_logo = true; # no anime
      };

      # Example windowrule v1
      # windowrule = float, ^(kitty)$
      # Example windowrule v2
      # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

      # See https://wiki.hyprland.org/Configuring/Keywords/ for more
      "$mainMod" = "SUPER";

      # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
      bind =
        [
          "$mainMod, Return, exec, kitty"
          "$mainMod SHIFT, Q, killactive,"
          "$mainMod SHIFT, C, exit,"
          "$mainMod, T, togglefloating,"
          "$mainMod, D, exec, rofi -show-icons -combi-modi windows,drun,run -show combi -modi windows:${myPkgs.hypr-window-select}/bin/hypr-window-select"
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

          "$mainMod, C, cyclenext"

          "$mainMod, bracketleft, focusmonitor, -1"
          "$mainMod, bracketright, focusmonitor, +1"

          "$mainMod CTRL, bracketleft, swapactiveworkspaces, current -1"
          "$mainMod CTRL, bracketright, swapactiveworkspaces, current +1"

          # Scroll through existing workspaces with mainMod + scroll
          "$mainMod, mouse_down, workspace, e+1"
          "$mainMod, mouse_up, workspace, e-1"
        ]
        ++ (builtins.concatMap (
          x:
          let
            ws = toString x;
            keyCode = toString (x + 9);
          in
          [
            "$mainMod, ${keyCode}, exec, ${myPkgs.hypr-greedy-focus}/bin/hypr-greedy-focus ${ws}"
            "$mainMod SHIFT, ${keyCode}, movetoworkspace,  ${ws}"
          ]
        ) (builtins.genList (x: x + 1) 10));

      # Move/resize windows with mainMod + LMB/RMB and dragging
      bindm = [
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];

      windowrulev2 = [
        "float,class:^(Vampire_Survivors)$"
        "workspace 1 silent,class:^(Google-chrome)$"
        "workspace 1 silent,class:^(firefox)$"
        "workspace 3 silent,class:^(jetbrains-idea)$"
        "workspace 5 silent,class:^(steam)$"
        "workspace 9 silent,class:^(KeepassXC)$"
        # fix steam menus
        "stayfocused, title:^()$,class:^(steam)$"
        "minsize 1 1, title:^()$,class:^(steam)$"
        "fullscreen,class:^(.gamescope-wrapped)$"
      ];
    };
  };
}
