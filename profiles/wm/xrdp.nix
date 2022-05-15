{ pkgs, ... }: {
  home.file."startwm.sh".source = pkgs.writeShellScript "startwm.sh" ''
    source /etc/profile

    on-exit() {
      systemctl --user stop xrdp-session.target
    }
    trap on-exit EXIT

    # fixes AltGr producing Left Arrow input on kitty
    export XKB_DEFAULT_RULES=base
    systemctl --user import-environment XKB_DEFAULT_RULES
    systemctl --user stop picom.service # picom doesn't work right on xrdp session
    systemctl --user start xrdp-session.target
    if [ "$DBUS_SESSION_BUS_ADDRESS" ]; then
      export DBUS_SESSION_BUS_ADDRESS
      exec ${pkgs.runtimeShell} ~/.xsession
    else
      exec ${pkgs.dbus}/bin/dbus-run-session ${pkgs.runtimeShell} ~/.xsession
    fi
  '';
  systemd.user.targets.xrdp-session = {
    Unit = { OnSuccess = [ "picom.service" ]; };
  };
  systemd.user.services.picom.Service.ExecCondition =
    (pkgs.writers.writeDash "no-xrdp.sh" ''
      ${pkgs.systemd}/bin/systemctl --user is-active --quiet xrdp-session.target && exit 1 || exit 0
    '').outPath;
}
