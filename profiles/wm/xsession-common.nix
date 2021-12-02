{ pkgs, lib, ... }: {
  services.unclutter = {
    enable = true;
    timeout = 10;
  };
  services.screen-locker = {
    lockCmd = lib.mkDefault "i3lock -n -t -f";
    xautolock.extraOptions = [ "-corners" "--00" ];
  };
  xsession.enable = lib.mkDefault true;
  xsession.initExtra = ''
    xset s off -dpms
    dbus-update-activation-environment --systemd DISPLAY
  '';
  xsession.pointerCursor = {
    name = "capitaine-cursors";
    package = pkgs.capitaine-cursors;
  };
}
