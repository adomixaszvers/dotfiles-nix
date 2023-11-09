{ pkgs, config, ... }: {
  imports = [ ../waybar ../dunst.nix ];
  home.packages = with pkgs; [
    grimblast
    pamixer
    swaylock
    swayidle
    wl-clipboard
    wdisplays
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
        modules-right =
          [ "pulseaudio" "cpu" "memory" "temperature" "clock" "tray" ];
        "clock" = { format = "{:%Y-%m-%d %H:%M}"; };
        "pulseaudio" = { scroll-step = "5.0"; };
        temperature.thermal-zone = config.gui.thermal-zone;
      };
      systemd = {
        enable = true;
        target = "hyprland-session.target";
      };
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
      env = [ "XCURSOR_SIZE,24" "KITTY_CONF_FONT,font_size 12.0" ];
      # sets xwayland scale

      # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
      input = {
        kb_layout = "lt,us";
        kb_options = "grp:caps_toggle";
        numlock_by_default = true;

        follow_mouse = 1;

        touchpad = { natural_scroll = false; };

        sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
      };

      general = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more

        gaps_in = 5;
        gaps_out = 20;
        border_size = 2;
        "col.active_border" = "rgba(33ccffee) rgba(00ff99ee) 45deg";
        "col.inactive_border" = "rgba(595959aa)";

        layout = "dwindle";
      };

      decoration = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more

        rounding = 10;
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
        enabled = true;

        # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

        bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";

        animation = [
          "windows, 1, 7, myBezier   "
          "windowsOut, 1, 7, default, popin 80%"
          "border, 1, 10, default    "
          "borderangle, 1, 8, default"
          "fade, 1, 7, default       "
          "workspaces, 1, 6, default "
        ];
      };

      dwindle = {
        # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
        pseudotile =
          true; # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
        preserve_split = true; # you probably want this
        no_gaps_when_only = true;
      };

      master = {
        # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
        new_is_master = true;
        no_gaps_when_only = true;
      };

      gestures = {
        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        workspace_swipe = false;
      };

      # Example per-device config
      # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
      "device:epic mouse V1" = { sensitivity = -0.5; };

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
      bind = [
        "$mainMod, Return, exec, kitty"
        "$mainMod SHIFT, Q, killactive,"
        "$mainMod SHIFT, C, exit,"
        "$mainMod, E, exec, dolphin"
        "$mainMod, V, togglefloating,"
        "$mainMod, D, exec, rofi -show drun"
        "$mainMod SHIFT, D, exec, rofi -show run"
        "$mainMod, F, fullscreen, 1"
        "$mainMod, P, pseudo," # dwindle
        "$mainMod, J, togglesplit," # dwindle
        "$mainMod, F4, exec, rofi-powermenu"

        ", Print, exec, grimblast copy output"
        "ALT, Print, exec, grimblast copy area"

        # Move focus with mainMod + arrow keys
        "$mainMod, left, movefocus, l"
        "$mainMod, right, movefocus, r"
        "$mainMod, up, movefocus, u"
        "$mainMod, down, movefocus, d"

        # Switch workspaces with mainMod + [0-9]
        "$mainMod, 10, workspace, 1"
        "$mainMod, 11, workspace, 2"
        "$mainMod, 12, workspace, 3"
        "$mainMod, 13, workspace, 4"
        "$mainMod, 14, workspace, 5"
        "$mainMod, 15, workspace, 6"
        "$mainMod, 16, workspace, 7"
        "$mainMod, 17, workspace, 8"
        "$mainMod, 18, workspace, 9"
        "$mainMod, 19, workspace, 10"

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        "$mainMod SHIFT, 10, movetoworkspace, 1"
        "$mainMod SHIFT, 11, movetoworkspace, 2"
        "$mainMod SHIFT, 12, movetoworkspace, 3"
        "$mainMod SHIFT, 13, movetoworkspace, 4"
        "$mainMod SHIFT, 14, movetoworkspace, 5"
        "$mainMod SHIFT, 15, movetoworkspace, 6"
        "$mainMod SHIFT, 16, movetoworkspace, 7"
        "$mainMod SHIFT, 17, movetoworkspace, 8"
        "$mainMod SHIFT, 18, movetoworkspace, 9"
        "$mainMod SHIFT, 19, movetoworkspace, 10"

        "$mainMod, bracketleft, focusmonitor, -1"
        "$mainMod, bracketright, focusmonitor, +1"

        "$mainMod CTRL, bracketleft, swapactiveworkspaces, current -1"
        "$mainMod CTRL, bracketright, swapactiveworkspaces, current +1"

        # Scroll through existing workspaces with mainMod + scroll
        "$mainMod, mouse_down, workspace, e+1"
        "$mainMod, mouse_up, workspace, e-1"
      ];

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
      ];
    };
  };
}
