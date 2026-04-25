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
    ../waybar
    ../dunst.nix
    (inputs.nix-wrapper-modules.lib.mkInstallModule {
      loc = [
        "home"
        "packages"
      ];
      name = "niri";
      value = inputs.self.wrapperModules.niri;
    })
  ];
  home.packages = [
    pkgs.nautilus # for file chooser dialogs
    pkgs.wl-clipboard
    pkgs.xwayland-satellite
  ];
  programs = {
    emacs.package = pkgs.emacs-pgtk;
    rofi = {
      extraConfig.modi = "drun,run,window,combi";
    };
    waybar = {
      systemd.enable = true;
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
  wrappers.niri = {
    enable = true;
    "config.kdl".path = "${config.xdg.configHome}/niri/config.kdl";
    settings = {
      binds."Mod+Ctrl+O".spawn = lib.getExe myPkgs.niri-swap-monitors;

      cursor = {
        xcursor-size = config.stylix.cursor.size;
        xcursor-theme = config.stylix.cursor.name;
      };
      layout.border = with config.lib.stylix.colors.withHashtag; {
        active-color = base0D;
        inactive-color = base03;
      };

    };
  };
  xdg.configFile."niri/config.kdl".source =
    config.wrappers.niri.constructFiles.generatedConfig.outPath;
  stylix.targets = {
    hyprlock.enable = true;
    hyprpaper.enable = true;
    waybar = {
      enable = true;
      enableCenterBackColors = true;
      enableLeftBackColors = true;
      enableRightBackColors = false;
    };
  };
}
