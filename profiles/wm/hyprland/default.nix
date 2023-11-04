{ pkgs, inputs, system, config, ... }: {
  imports = [ ../waybar ../dunst.nix ];
  home.packages =
    (with pkgs; [ pamixer swaylock swayidle wl-clipboard wdisplays ])
    ++ [ inputs.hyprland-contrib.packages.${system}.grimblast ];
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
        modules-left = [ "wlr/workspaces" ];
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
    extraConfig = builtins.readFile ./hyprland.conf + ''
      monitor=eDP-1,1920x1080,0x0,1.25
    '';
  };
}
