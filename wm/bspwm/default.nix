{ pkgs, config, ... }: {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ./sxhkd.nix ];
  home.packages = with pkgs; [ bspwm mine.bspwm-reorder-desktops tdrop ];
  services.polybar.config = let
    common = {
      modules-left = "bspwm divider title";
      wm-restack = "bspwm";
    };
  in {
    "bar/top" = common;
    "bar/top-extra" = common;
    "module/bspwm" = let colors = config.colors;
    in {
      type = "internal/bspwm";
      pin-workspaces = false;
      enable-click = true;

      format = "<label-state> <label-mode>";

      label = "%{T2}%title%%{T-}";
      label-focused = "%{T2}%name%%{T-}";
      label-focused-overline = colors.foreground;

      label-occupied = "%{T2}%name%%{T-}";

      label-empty = "%name%";
      label-empty-foreground = colors.white;

      label-separator = " ";

      label-monocle = "%{T2}%{T-}";
      label-tiled = "%{T2}%{T-}";
      label-fullscreen = "%{T2}%{T-}";
      label-floating = "%{T2}%{T-}";
      label-pseudotiled = "%{T2}P%{T-}";
      label-locked = "%{T2}%{T-}";
      label-locked-foreground = "#bd2c40";
      label-sticky = "%{T2}%{T-}";
      label-sticky-foreground = "#fba922";
      label-private = "%{T2}%{T-}";
      label-private-foreground = "#bd2c40";
      label-marked = "%{T2}M%{T-}";
    };
  };
  xsession.windowManager.bspwm = {
    enable = true;
    extraConfig = ''
      feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png
      systemctl --user restart polybar.service
    '';
    monitors = { "'^1'" = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" ]; };
    rules = {
      Google-chrome = { desktop = "1"; };
      Firefox = { desktop = "1"; };
      jetbrains-idea = { desktop = "3"; };
      rambox = { desktop = "4"; };
      SmartGit = { desktop = "5"; };
      Steam = { desktop = "5"; };
      libreoffice = { desktop = "6"; };
      KeePassXC = { desktop = "9"; };
      "'Google Play Music Desktop Player'" = { desktop = "10"; };
      Spotify = { desktop = "10"; };
      "'*:scratchpad'" = { state = "floating"; };
    };
    settings = {
      border_width = 2;
      borderless_monocle = true;
      focus_follows_pointer = true;
      focused_border_color = config.colors.cyanb;
      gapless_monocle = true;
      pointer_follows_monitor = true;
      single_monocle = true;
      split_ratio = 0.52;
      top_padding = 16;
      window_gap = 7;
    };
  };
}
