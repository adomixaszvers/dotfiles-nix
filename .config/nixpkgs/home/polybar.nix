{ pkgs, config, ... }:
with config.lib.colors.solarized;
{
  services.polybar = {
    enable = false;
    package = pkgs.polybar.override {
      i3GapsSupport = true;
      alsaSupport = true;
      pulseSupport = true;
    };
    config = {
      "bar/bottom" = {
        inherit foreground background;
        font-0 = "DejaVuSansMono Nerd Font:pixelsize=10;0";
        bottom = true;
        width = "100%";
        heigth = "3%";
        modules-center = "title";
        modules-left = "i3";
        modules-right = "pulseaudio";
      };
      "module/pulseaudio" = {
        type = "internal/pulseaudio";
      };
      "module/i3" = {
        type = "internal/i3";
        # Available tokens:
        #   %mode%
        # Default: %mode%
        label-mode = "%mode%";
        label-mode-padding = 2;
        label-mode-background = "#e60053";

        # Available tokens:
        #   %name%
        #   %icon%
        #   %index%
        #   %output%
        # Default: %icon%  %name%
        label-focused = "%icon%  %name%";
        label-focused-foreground = "#ffffff";
        label-focused-background = "#3f3f3f";
        label-focused-underline = "#fba922";
        label-focused-padding = 1;

        # Available tokens:
        #   %name%
        #   %icon%
        #   %index%
        #   %output%
        # Default: %icon%  %name%
        label-unfocused = "%icon%  %name%";
        label-unfocused-padding = 4;

        # Available tokens:
        #   %name%
        #   %icon%
        #   %index%
        #   %output%
        # Default: %icon%  %name%
        label-visible = "%index%";
        label-visible-underline = "#555555";
        label-visible-padding = 4;

        # Available tokens:
        #   %name%
        #   %icon%
        #   %index%
        #   %output%
        # Default: %icon%  %name%
        label-urgent = "%index%";
        label-urgent-foreground = "#000000";
        label-urgent-background = "#bd2c40";
        label-urgent-padding = 4;

        # Separator in between workspaces
        label-separator = "|";
        label-separator-padding = 2;
        label-separator-foreground = "#ffb52a";
      };
      "module/title" = {
        type = "internal/xwindow";
      };
    };
    script = ''
      export PATH=$PATH:${pkgs.i3-gaps}/bin

      # Terminate already running bar instances
      ${pkgs.psmisc}/bin/killall -q polybar

      # Wait until the processes have been shut down
      while ${pkgs.procps}/bin/pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

      # Launch Polybar, using default config location ~/.config/polybar/config
      polybar bottom &

      echo "Polybar launched..."
    '';
  };
}
