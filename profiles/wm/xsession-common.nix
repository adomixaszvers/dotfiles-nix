{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [ ./xinitrc.nix ];
  programs.autorandr.enable = true;
  services = {
    unclutter = {
      enable = lib.mkDefault true;
      timeout = 10;
    };
    screen-locker = {
      lockCmd =
        let
          dpmsTimeoutSeconds = 60;
          lockScreen = pkgs.writers.writeDash "lock-screen.sh" ''
            PATH=${
              lib.makeBinPath [
                pkgs.i3lock-color
                pkgs.xorg.xset
              ]
            }:$PATH
            DPMS_TIMEOUT=${toString dpmsTimeoutSeconds}
            revert() {
              xset dpms 0 0 0
            }
            trap revert HUP INT TERM
            xset +dpms dpms $DPMS_TIMEOUT $DPMS_TIMEOUT $DPMS_TIMEOUT
            i3lock-color -n -t -c ${config.lib.stylix.colors.base00} -f --pass-media-keys
            revert
          '';
        in
        lib.mkDefault lockScreen.outPath;
      xautolock.extraOptions = [
        "-corners"
        "--00"
      ];
    };
  };
  xsession = {
    enable = lib.mkDefault true;
    initExtra = # bash
      ''
        autorandr --change
        dbus-update-activation-environment --systemd DISPLAY
      '';
    numlock.enable = true;
  };
  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
  };
  home.sessionVariables.KITTY_CONF_FONT = "font_size 9";
}
