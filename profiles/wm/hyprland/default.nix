{ pkgs, inputs, system, ... }: {
  imports = [ ../waybar ../dunst.nix ];
  home.packages =
    (with pkgs; [ gamescope pamixer swaylock swayidle wl-clipboard wdisplays ])
    ++ [ inputs.hyprland-contrib.packages.${system}.grimblast ];
  programs.emacs.package = let
    emacsNixpkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [ inputs.emacs-overlay.overlay ];
    };
  in emacsNixpkgs.emacsPgtk;
  programs.rofi = {
    package = pkgs.rofi-wayland;
    extraConfig.modi = "drun,run,ssh";
  };
  programs.waybar = {
    package = inputs.hyprland.packages.${system}.waybar-hyprland;
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
      temperature.thermal-zone = 0;
    };
    systemd = {
      enable = true;
      target = "hyprland-session.target";
    };
  };
  wayland.windowManager.hyprland = {
    enable = true;
    xwayland = {
      enable = true;
      hidpi = true;
    };
    extraConfig = builtins.readFile ./hyprland.conf;
  };
}
