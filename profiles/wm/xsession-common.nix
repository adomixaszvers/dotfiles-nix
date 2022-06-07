{ pkgs, config, lib, ... }: {
  imports = [ ./xinitrc.nix ];
  programs.autorandr.enable = true;
  services.unclutter = {
    enable = true;
    timeout = 10;
  };
  services.screen-locker = {
    lockCmd = let
      dpmsTimeoutSeconds = 10;
      lockScreen = pkgs.writers.writeDash "lock-screen.sh" ''
        PATH=${lib.makeBinPath [ pkgs.i3lock pkgs.xorg.xset ]}:$PATH
        DPMS_TIMEOUT=${toString dpmsTimeoutSeconds}
        revert() {
          xset dpms 0 0 0
        }
        trap revert HUP INT TERM
        xset +dpms dpms $DPMS_TIMEOUT $DPMS_TIMEOUT $DPMS_TIMEOUT
        i3lock -n -t -c ${builtins.substring 1 6 config.colors.background} -f
        revert
      '';
    in lib.mkDefault lockScreen.outPath;
    xautolock.extraOptions = [ "-corners" "--00" ];
  };
  xsession.enable = lib.mkDefault true;
  xsession.initExtra = ''
    autorandr --change
    xset s off -dpms
    dbus-update-activation-environment --systemd DISPLAY
  '';
  xsession.numlock.enable = true;
  home.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
    gtk.enable = true;
    x11.enable = true;
  };
}
