{ pkgs, config, lib, ... }:
let
  colors = config.colors // (with config.colors; {
    custom-foreground = foreground;
    custom-background-dark = background;
    custom-background-light = blackb;
    custom-primary = cyanb;
    custom-warn = redb;
  });
  module = icon: other:
    {
      format-prefix = "%{T2}${icon}%{T-}";
      format-prefix-foreground = colors.custom-primary;
      format-prefix-background = colors.custom-background-dark;
      label-foreground = colors.custom-foreground;
      label-background = colors.custom-background-dark;
    } // other;
  defaultBar = {
    modules-left = lib.mkDefault "ewmh";
    modules-center = lib.mkDefault "";

    dpi = ''
      ''${xrdb:Xft.dpi:96}
    '';

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
    font-2 = "Noto Color Emoji:fontformat=truetype:scale=12:antialias=true;1";

    line-size = 1;
  };
in {
  services.trayer = {
    enable = true;
    settings = {
      monitor = "primary";
      distance = 16;
      tint = builtins.replaceStrings [ "#" ] [ "0x" ] colors.background;
      alpha = 0;
      edge = "top";
      align = "right";
      expand = false;
      transparent = true;
      widthtype = "request";
      height = 16;
    };
  };
  services.polybar = {
    enable = lib.mkDefault true;
    package = pkgs.polybarFull;
    config = {
      "bar/top" = defaultBar // {
        enable-ipc = true;
        modules-right = lib.mkDefault
          "memory divider disk divider cpu divider temperature divider volume divider keyboard divider date divider time divider trayer";
      };
      "bar/top-extra" = defaultBar // {
        modules-right =
          lib.mkDefault "keyboard divider date divider time divider";
      };
      "module/cpu" = module "󰘚" {
        type = "internal/cpu";
        label = " %percentage:3%%";
      };
      "module/date" = module "󰙹" {
        type = "internal/date";
        date = "%Y-%m-%d%";
        label = " %date%";
      };
      "module/time" = module "󰥔" {
        type = "internal/date";

        time = "%H:%M";
        time-alt = "%H:%M:%S";

        label = " %time%";
      };
      "module/disk" = {
        type = "internal/fs";
        mount-0 = "/";

        label-mounted = " %mountpoint% %free%";
        label-mounted-foreground = colors.custom-foreground;
        label-mounted-background = colors.custom-background-dark;
      };
      "module/ewmh" = {
        type = "internal/xworkspaces";
        label-active = "[%name%]";
        label-active-background = colors.custom-primary;
        label-active-foreground = colors.custom-background-dark;
        label-occupied = "%name%";
        label-occupied-padding = 1;
        label-empty = "%name%";
        label-empty-padding = 1;
      };
      "module/divider" = {
        type = "custom/text";
        content = "|";
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

      "module/keyboard" = module " " {
        type = "internal/xkeyboard";
        label-indicator-on-numlock = "N";
      };
      "module/memory" = module "󰍛" {
        type = "internal/memory";
        label = " %percentage_used:3%%";
      };
      "module/temperature" = module "󰈸" {
        type = "internal/temperature";
        warn-temperature = 70;
        label = " %temperature-c:3%";

        format-warn-prefix = "%{T3}󰈸%{T-}";
        format-warn-prefix-foreground = colors.custom-warn;
        format-warn-prefix-background = colors.custom-background-dark;

        label-warn = " %temperature-c:3%";
        label-warn-foreground = colors.custom-foreground;
        label-warn-background = colors.custom-background-dark;
      };
      "module/volume" = {
        type = "internal/pulseaudio";

        format-volume-prefix = "%{T3}󰕾%{T-}";
        format-volume-prefix-foreground = colors.custom-primary;
        format-volume-prefix-background = colors.custom-background-dark;

        label-volume = " %percentage:3%%";
        label-volume-foreground = colors.custom-foreground;
        label-volume-background = colors.custom-background-dark;

        format-muted-prefix = "%{T3}󰝟%{T-}";
        format-muted-prefix-foreground = colors.custom-warn;
        format-muted-prefix-background = colors.custom-background-dark;

        label-muted = " %percentage:3%%";
        label-muted-foreground = colors.custom-foreground;
        label-muted-background = colors.custom-background-dark;
      };

      "module/title" = {
        type = "internal/xwindow";
        label = "%{T2}%title%%{T-}";
        label-maxlen = 75;
      };
      "module/trayer" = let
        script = pkgs.writeShellScript "toggle-trayer" ''
          PATH=$PATH:${lib.makeBinPath [ pkgs.xdo ]}

          filename="$XDG_RUNTIME_DIR/trayer-hidden$DISPLAY"

          if [ ! -f "$filename" ]; then
            touch "$filename"
            xdo hide -N trayer
            polybar-msg action "#trayer.hook.1"
          else
            rm "$filename"
            xdo show -N trayer 
            xdo raise -N trayer 
            polybar-msg action "#trayer.hook.0"
          fi
        '';
        trayer-init = pkgs.writeShellScript "trayer-init" ''
          if [ -f "$XDG_RUNTIME_DIR/trayer-hidden$DISPLAY" ]; then
            echo " "
            ${pkgs.xdo}/bin/xdo hide -N trayer
          else
            echo " "
          fi
        '';
      in {
        type = "custom/ipc";
        hook-0 = ''echo " "'';
        hook-1 = ''echo " "'';
        hook-2 = toString trayer-init;
        click-left = toString script;
        initial = 2;
      };
    };
    script = ''
      PATH=$PATH:${
        with pkgs;
        lib.makeBinPath [
          coreutils
          gawk
          gnugrep
          procps
          psmisc
          xorg.xrandr
          xdotool
        ]
      }

      # Terminate already running bar instances
      killall -q polybar

      # Wait until the processes have been shut down
      while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

      XRANDR_QUERY="$(xrandr --query | grep " connected")"
      XRANDR_ACTIVE="$(xrandr --listactivemonitors)"

      launch_polybar() {
        MONITOR=$1
        BAR=$2
        SCREEN_ID=$(echo "$XRANDR_ACTIVE"| awk "/ $MONITOR/ { print substr(\$1, 0, 1) }")
        MONITOR="$MONITOR" SCREEN_ID="$SCREEN_ID" polybar "$BAR"
      }

      if [ "$(echo "$XRANDR_QUERY" | wc -l)" -eq 1 ]; then
        MONITOR=$(echo "$XRANDR_QUERY" | cut -d' ' -f1)
        launch_polybar "$MONITOR" top &
      else
        PRIMARY_MONITOR=$(echo "$XRANDR_QUERY"| grep primary | cut -d" " -f1)

        launch_polybar "$PRIMARY_MONITOR" top &

        for m in $(echo "$XRANDR_QUERY"| grep -v primary | cut -d' ' -f1); do
            launch_polybar "$m" top-extra &
        done
      fi


      echo "Polybar launched..."
    '';
  };
}
