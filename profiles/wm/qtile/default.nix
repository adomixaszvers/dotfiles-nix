{ pkgs, myPkgs, ... }:

{
  imports = [ ../dunst.nix ];
  home.packages = [ pkgs.qtile pkgs.wofi myPkgs.rofi-powermenu ];
  # services.pasystray.enable = true;
  services.kanshi.enable = true;
  xsession.windowManager.command = "qtile start";
  home.sessionVariables = { XKB_DEFAULT_LAYOUT = "lt,us"; };
  xdg.configFile."qtile/config.py" = {
    source = ./config.py;
    onChange = "qtile cmd-obj -o cmd -f reload_config 2>/dev/null || true";
  };
  xdg.configFile."qtile/autostart.sh".source =
    pkgs.writers.writeDash "autostart.sh" ''
      if [ -n "$WAYLAND_DISPLAY" ]; then
        ${pkgs.kanshi}/bin/kanshi &
      fi
    '';
}
