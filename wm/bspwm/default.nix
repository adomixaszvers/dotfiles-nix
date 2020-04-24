{ pkgs, config, ... }: {
  imports = [ ../polybar.nix ../dunst.nix ../picom.nix ./sxhkd.nix ];
  home.packages = with pkgs; [ bspwm mine.bspwm-reorder-desktops tdrop wmname ];
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
      label-focused = "[%name%]";

      label-occupied = "%name%";

      label-empty = "%name%";
      label-empty-foreground = colors.cyanb;

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
  xsession.windowManager.bspwm = {
    enable = true;
    extraConfig = ''
      wmname LG3D
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
