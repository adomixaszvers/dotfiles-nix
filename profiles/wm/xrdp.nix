{
  pkgs,
  config,
  lib,
  ...
}:
{
  home.file."startwm.sh".source = pkgs.writeShellScript "startwm.sh" ''
    source /etc/profile

    # fixes AltGr producing Left Arrow input on kitty
    export XKB_DEFAULT_RULES=base
    systemctl --user import-environment XKB_DEFAULT_RULES
    systemctl --user stop picom.service # picom doesn't work right on xrdp session
    export NH_HOME_FLAKE="git+file://$HOME/.config/nixpkgs#homeConfigurations.thinkpad-home.activationPackage"
    if [ -n "$DBUS_SESSION_BUS_ADDRESS" ]; then
      export DBUS_SESSION_BUS_ADDRESS
      exec ${pkgs.runtimeShell} ~/.xsession
    else
      exec ${pkgs.dbus}/bin/dbus-run-session ${pkgs.runtimeShell} ~/.xsession
    fi
  '';
  systemd.user.services.picom = lib.optionalAttrs config.services.picom.enable {
    Unit = {
      StartLimitBurst = 3;
      StartLimitIntervalSec = 60;
    };
  };
}
