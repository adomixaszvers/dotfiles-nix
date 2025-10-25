{
  config,
  inputs,
  pkgs,
  lib,
  myPkgs,
  ...
}:
{
  imports = [
    inputs.niri.homeModules.niri
    inputs.niri.homeModules.stylix
    ../waybar
    ../dunst.nix
  ];
  home.packages = [
    pkgs.xwayland-satellite
    myPkgs.niri-swap-monitors
  ];
  xdg.configFile."niri/config.kdl".source =
    config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.config/nixpkgs/profiles/wm/niri/config.kdl";
  programs = {
    niri = {
      enable = true;
      config = null;
      package = pkgs.niri;
    };
    rofi = {
      package = pkgs.rofi-wayland;
      # modes = [
      #   "run"
      #   "drun"
      #   "combi"
      #   {
      #     name = "windows";
      #     path = lib.getExe myPkgs.niri-window-select;
      #   }
      # ];
      extraConfig.modi = "drun,run,windows:${lib.getExe myPkgs.niri-window-select},combi";
    };
    waybar = {
      settings.mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [ "niri/workspaces" ];
        modules-center = [ "niri/window" ];
        modules-right = (lib.optional config.gui.hasBattery "battery") ++ [
          "niri/language"
          "pulseaudio"
          "cpu"
          "memory"
          "temperature"
          "clock"
          "tray"
        ];
        "niri/language" = {
          format-lt = "lt";
          format-en = "us";
        };
        "niri/window" = {
          # format = "{title:.100}";
          separate-outputs = true;
        };
        temperature.thermal-zone = config.gui.thermal-zone;
      };
      style = # css
        ''
          window#waybar.fullscreen #window {
            border-radius: 8px;
          }

          /* see https://github.com/Alexays/Waybar/issues/2793#issuecomment-2039369688 */
          #language {
            min-width: 20px;
          }
        '';
    };
  };
  stylix.targets = {
    hyprlock.enable = true;
    hyprpaper.enable = true;
    niri.enable = false;
    waybar = {
      enable = true;
      enableCenterBackColors = true;
      enableLeftBackColors = true;
      enableRightBackColors = false;
    };
  };
}
