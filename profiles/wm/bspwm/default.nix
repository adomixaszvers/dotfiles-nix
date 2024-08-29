{
  pkgs,
  myPkgs,
  config,
  ...
}:
{
  imports = [
    ../polybar.nix
    ../dunst.nix
    ../picom.nix
    ./sxhkd.nix
  ];
  home.packages =
    (with pkgs; [
      pamixer
      tdrop
      wmname
    ])
    ++ (with myPkgs; [
      bspwm-reorder-desktops
      bspwm-greedy-focus
    ]);
  services.polybar.config =
    let
      common = {
        modules-left = "bspwm divider title";
        wm-restack = "bspwm";
      };
    in
    {
      "bar/top" = common;
      "bar/top-extra" = common;
      "module/bspwm" = {
        type = "internal/bspwm";
        pin-workspaces = true;
        enable-click = false;

        format = "<label-state> <label-mode>";

        label = "%{T2}%title%%{T-}";
        label-focused = "[%name%]";

        label-occupied = "%name%";

        label-empty = "%name%";
        label-empty-foreground = config.lib.stylix.colors.base0C;

        label-separator = " ";

        label-monocle = "%{T2}M%{T-}";
        label-tiled = "%{T2}t%{T-}";
        label-fullscreen = "%{T2}F%{T-}";
        label-floating = "%{T2}f%{T-}";
        label-pseudotiled = "%{T2}P%{T-}";
        label-locked = "%{T2}L%{T-}";
        label-locked-foreground = "#bd2c40";
        label-sticky = "%{T2}s%{T-}";
        label-sticky-foreground = "#fba922";
        label-private = "%{T2}p%{T-}";
        label-private-foreground = "#bd2c40";
        label-marked = "%{T2}m%{T-}";
      };
    };
  stylix.targets.bspwm.enable = true;
  xsession.windowManager.bspwm = {
    enable = true;
    extraConfig = # bash
      ''
        wmname LG3D

        declare -i last_called=0
        declare -i throttle_by=2

        throttle() {
          local -i now=$SECONDS
          if ((now - last_called >= throttle_by))
          then
            "$@"
          fi
          last_called=$SECONDS
        }

        refresh () {
          feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png
          systemctl --user restart polybar.service
        }

        init_desktops () {
          # xrandr outputs a heading
          local monitors=($(xrandr --listactivemonitors| awk 'NR!=1 { print $4 }' ))

          case "''${#monitors[@]}" in
            2)
              bspc monitor ''${monitors[0]} -d 1 2 3 4 5
              bspc monitor ''${monitors[1]} -d 6 7 8 9 10
              ;;
            3)
              bspc monitor ''${monitors[0]} -d 1 2 3 4
              bspc monitor ''${monitors[1]} -d 5 6 7
              bspc monitor ''${monitors[2]} -d 8 9 10
              ;;
            *)
              bspc monitor ''${monitors[0]} -d 1 2 3 4 5 6 7 8 9 10
              ;;
          esac
        }

        init_desktops
        refresh

        bspc subscribe monitor_geometry | while read -r geometry_event; do
          throttle refresh
        done &

        bspc subscribe monitor_add | while read -r add_event; do
          bspc desktop 'any.!active' -m "$( echo $add_event | awk '{ print $2 }')"
          bspc desktop Desktop -r
          bspwm-reorder-desktops
        done &

        bspc subscribe monitor_remove | while read -r remove_event; do
          bspwm-reorder-desktops
        done &
      '';
    rules = {
      Google-chrome = {
        desktop = "1";
      };
      firefox = {
        desktop = "1";
      };
      jetbrains-idea = {
        desktop = "3";
      };
      Rambox = {
        desktop = "4";
      };
      discord = {
        desktop = "4";
      };
      SmartGit = {
        desktop = "5";
      };
      steam = {
        desktop = "5";
      };
      "*:libreoffice" = {
        desktop = "6";
      };
      "org.remmina.Remmina" = {
        desktop = "8";
      };
      KeePassXC = {
        desktop = "9";
      };
      "Google Play Music Desktop Player" = {
        desktop = "10";
      };
      Spotify = {
        desktop = "10";
      };
      "*:scratchpad" = {
        state = "fullscreen";
      };
      lxqt-openssh-askpass = {
        state = "floating";
      };
    };
    settings = {
      border_width = 2;
      borderless_monocle = true;
      focus_follows_pointer = true;
      gapless_monocle = true;
      pointer_follows_monitor = true;
      single_monocle = true;
      split_ratio = 0.52;
      top_padding = 16;
      window_gap = 7;
      remove_unplugged_monitors = true;
      remove_disabled_monitors = true;
    };
  };
}
