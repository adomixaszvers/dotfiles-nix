{ pkgs, config, lib, ... }:
let
  colors = config.lib.colors // (with config.lib.colors; {
    custom-foreground = foreground;
    custom-background-dark = background;
    custom-background-light = blackb;
    custom-primary = cyanb;
    custom-warn = redb;
  });
  defaultBar = {
    modules-left = lib.mkDefault "ewmh";

    monitor = ''
      ''${env:MONITOR}
    '';
    height = 16;
    bottom = false;
    fixed-center = true;

    inherit (colors) background foreground;

    font-0 =
      "NotoMono Nerd Font:fontformat=truetype:pixelsize=8:antialias=true;1";
    font-1 = "NotoMono Nerd Font:fontformat=truetype:size=10:antialias=true;1";
    font-2 = "Material Icons:fontformat=truetype:pixelsize=10:antialias=true;2";

    line-size = 1;
  };
in {
  services.polybar = {
    enable = lib.mkDefault true;
    package = pkgs.polybar.override { pulseSupport = true; };
    config = {
      "bar/top" = defaultBar // {
        modules-center = "";
        modules-right =
          "memory divider disk divider cpu divider temperature divider volume divider date divider time divider";
        tray-position = "right";
        tray-max-size = 16;
        tray-background = colors.custom-background-dark;
      };
      "bar/top-extra" = defaultBar // {
        modules-center = "";
        modules-right = "";
      };
      "module/cpu" = {
        type = "internal/cpu";

        format-prefix = "%{T3}%{T-}";
        format-prefix-foreground = colors.custom-primary;
        format-prefix-background = colors.custom-background-dark;

        label = " %percentage:3%%";
        label-foreground = colors.custom-foreground;
        label-background = colors.custom-background-dark;

      };
      "module/date" = {
        type = "internal/date";
        date = "%Y-%m-%d%";
        format-prefix = "%{T3}%{T-}";
        format-prefix-foreground = colors.custom-primary;
        format-prefix-background = colors.custom-background-dark;

        label = " %date%";
        label-foreground = colors.custom-foreground;
        label-background = colors.custom-background-dark;
      };
      "module/time" = {
        type = "internal/date";

        time = "%H:%M";
        time-alt = "%H:%M:%S";

        format-prefix = "%{T3}%{T-}";
        format-prefix-foreground = colors.custom-primary;
        format-prefix-background = colors.custom-background-dark;

        label = " %time%";
        label-foreground = colors.custom-foreground;
        label-background = colors.custom-background-dark;

      };
      "module/disk" = {
        type = "internal/fs";
        mount-0 = "/";
        # mount-1 = "/home";
        format-mounted-prefix = "%{T3}%{T-}";
        format-mounted-prefix-foreground = colors.custom-primary;
        format-mounted-prefix-background = colors.custom-background-dark;

        label-mounted = " %mountpoint%%percentage_used%%";
        label-mounted-foreground = colors.custom-foreground;
        label-mounted-background = colors.custom-background-dark;
      };
      "module/ewmh" = {
        type = "internal/xworkspaces";
        label-active = "%icon%";
        label-active-background = colors.custom-primary;
        label-active-foreground = colors.custom-background-dark;
        label-occupied = "%name%";
        label-empty = "%icon%";
        icon-0 = "1;%{T3}%{T-} :: %{T3}%{T-}";
        icon-1 = "2;%{T3}%{T-} :: %{T3}%{T-}";
        icon-2 = "3;%{T3}%{T-} :: %{T3}%{T-}";
        icon-3 = "4;%{T3}%{T-} :: %{T3}%{T-}";
        icon-4 = "5;%{T3}%{T-} :: %{T3}%{T-}";
        icon-5 = "6;%{T3}%{T-} :: %{T3}%{T-}";
        icon-6 = "7;%{T3}%{T-} :: %{T3}%{T-}";
        icon-7 = "8;%{T3}%{T-} :: %{T3}%{T-}";
        icon-8 = "9;%{T3}%{T-} :: %{T3}%{T-}";
      };
      "module/divider" = {
        type = "custom/text";
        content = "  |  ";
        content-foreground = colors.custom-background-light;
        content-background = colors.custom-background-dark;
      };
      "module/left_end" = {
        type = "custom/text";
        content = "   -----[   ";
        content-foreground = colors.custom-background-light;
        content-background = colors.custom-background-dark;
      };

      "module/right_end" = {

        type = "custom/text";

        content = "   ]-----   ";
        content-foreground = colors.custom-background-light;
        content-background = colors.custom-background-dark;
      };
      "module/i3" = {
        type = "internal/i3";

        strip-wsnumbers = false;

        label-mode-background = colors.background;

        label-unfocused-padding = 1;

        label-focused-background = colors.background;
        label-focused-foreground = colors.whiteb;
        label-focused-padding = 1;

        label-seperator-padding = 0;
      };

      "module/keyboard" = { type = "internal/xkeyboard"; };
      "module/memory" = {
        type = "internal/memory";
        format-prefix = "%{T3}%{T-}";
        format-prefix-foreground = colors.custom-primary;
        format-prefix-background = colors.custom-background-dark;

        label = " %percentage_used:3%%";
        label-foreground = colors.custom-foreground;
        label-background = colors.custom-background-dark;
      };
      "module/temperature" = {
        type = "internal/temperature";
        warn-temperature = 70;

        format-prefix = "%{T3}%{T-}";
        format-prefix-foreground = colors.custom-primary;
        format-prefix-background = colors.custom-background-dark;

        label = " %temperature-c:3%";
        label-foreground = colors.custom-foreground;
        label-background = colors.custom-background-dark;

        format-warn-prefix = "%{T3}%{T-}";
        format-warn-prefix-foreground = colors.custom-warn;
        format-warn-prefix-background = colors.custom-background-dark;

        label-warn = " %temperature-c:3%";
        label-warn-foreground = colors.custom-foreground;
        label-warn-background = colors.custom-background-dark;
      };
      "module/volume" = {
        type = "internal/pulseaudio";

        format-volume-prefix = "%{T3}%{T-}";
        format-volume-prefix-foreground = colors.custom-primary;
        format-volume-prefix-background = colors.custom-background-dark;

        label-volume = " %percentage:3%%";
        label-volume-foreground = colors.custom-foreground;
        label-volume-background = colors.custom-background-dark;

        format-muted-prefix = "%{T3}%{T-}";
        format-muted-prefix-foreground = colors.custom-warn;
        format-muted-prefix-background = colors.custom-background-dark;

        label-muted = " %percentage:3%%";
        label-muted-foreground = colors.custom-foreground;
        label-muted-background = colors.custom-background-dark;
      };

      "module/title" = {
        type = "internal/xwindow";
        label = "%{T2}%title%%{T-}";
        label-maxlen = 50;
      };
    };
    script = ''
      PATH=$PATH:${
        with pkgs;
        lib.makeBinPath [ coreutils gnugrep procps psmisc xorg.xrandr ]
      }

      # Terminate already running bar instances
      killall -q polybar

      # Wait until the processes have been shut down
      while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

      PRIMARY_MONITOR=$(xrandr --query | grep " connected primary" | cut -d" " -f1)

      # Launch Polybar, using default config location ~/.config/polybar/config
      MONITOR=$PRIMARY_MONITOR polybar top &

      for m in $(xrandr --query | grep " connected" | grep -v primary | cut -d" " -f1); do
          MONITOR=$m polybar top-extra &
      done

      echo "Polybar launched..."
    '';
  };
}
