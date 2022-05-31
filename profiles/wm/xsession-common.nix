{ pkgs, config, lib, ... }: {
  imports = [ ./xinitrc.nix ];
  programs.autorandr.enable = true;
  services.unclutter = {
    enable = true;
    timeout = 10;
  };
  services.screen-locker = {
    lockCmd = lib.mkDefault "${pkgs.i3lock}/bin/i3lock -n -t -c ${
        builtins.substring 1 6 config.colors.background
      } -f";
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
