{ pkgs, config, ... }:
let colors = config.lib.colors;
in
{
  services.polybar = {
    enable = true;
    package = pkgs.polybar.override {
      i3GapsSupport = true;
      pulseSupport = true;
    };
    config = {
      "bar/bottom" = {
        bottom = true;
        font-0 = "FuraMono Nerd Font Mono:pixelsize=10;2";
        width = "100%";
        height = 16;
        radius = 0;
        background = colors.black;
        foreground = colors.white;
        separator = " ";
        modules-left = "i3";
        modules-center = "title";
        modules-right = "cpu memory disk keyboard pulse date";

        tray-position = "right";
      };
      "module/cpu" = {
        type = "internal/cpu";
        label = "%percentage%";
      };
      "module/date" = {
        type = "internal/date";
        date = "%Y-%m-%d%";
        time = "%H:%M";
        format = "<label>";
        label = "%date% %time%";
      };
      "module/disk" = {
        type = "internal/fs";
        mount-0 = "/";
        mount-1 = "/home";
        label-mounted = "%mountpoint%%percentage_used%";
      };
      "module/i3" = {
        type = "internal/i3";

        strip-wsnumbers = true;

        label-mode-background = colors.background;

        label-unfocused-padding = 1;

        label-focused-background = colors.background;
        label-focused-foreground = colors.whiteb;
        label-focused-padding = 1;

        label-seperator-padding = 0;
      };

      "module/keyboard" = {
        type = "internal/xkeyboard";
      };
      "module/memory" = {
        type = "internal/memory";
        label = "%percentage_used%";
      };
      "module/pulse" = {
        type = "internal/pulseaudio";
        interval = 2;
        format-volume = "<ramp-volume><label-volume>";

        ramp-volume-0 = "奄";
        ramp-volume-1 = "奔";
        ramp-volume-2 = "墳";
        label-muted = "%percentage%";
        label-volume = "%percentage%";
      };

      "module/title" = {
        type = "internal/xwindow";
      };
    };
    script = ''
      #!/bin/bash
      PATH=$PATH:${pkgs.lib.makeBinPath [pkgs.procps pkgs.psmisc]}

      # Terminate already running bar instances
      killall -q polybar

      # Wait until the processes have been shut down
      while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

      # Launch Polybar, using default config location ~/.config/polybar/config
      polybar bottom &

      echo "Polybar launched..."
    '';
  };
}
