{ pkgs, lib, ... }:

{
  imports = [ ./work.nix ];
  home.file."startwm.sh".source = pkgs.writeShellScript "startwm.sh" ''
    source /etc/profile
    # fixes AltGr producing Left Arrow input on kitty
    export XKB_DEFAULT_RULES=base
    systemctl --user import-environment XKB_DEFAULT_RULES
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
      export DBUS_SESSION_BUS_ADDRESS
      exec ${pkgs.runtimeShell} ~/.xsession
    else
      exec ${pkgs.dbus}/bin/dbus-run-session ${pkgs.runtimeShell} ~/.xsession
    fi
  '';
  programs.rofi.theme = lib.mkForce "Arc";
  services.gpg-agent.enable = lib.mkForce false;
  services.picom.enable = false;
  services.screen-locker.enable = lib.mkForce false;
  services.sxhkd.keybindings = {
    "super + ctrl + r" = "bspc wm -r; bspc wm -o";
  };
  xsession.windowManager.bspwm.monitors = {
    "rdp0" = [ "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" ];
  };
}
