{ config, lib, ... }:
{
  networking.firewall.interfaces = {
    wg0.allowedTCPPorts = [ config.services.xrdp.port ];
    ztzlgoe57z.allowedTCPPorts = [ config.services.xrdp.port ];
  };
  services.xrdp = {
    enable = true;
    extraConfDirCommands =
      let
        cfg = config.services.xrdp;
      in
      # bash
      ''
        cp ${builtins.path { path = "${./km-00010427.ini}"; }} $out/km-00010427.ini
        cp ${builtins.path { path = "${./xrdp_keyboard.ini}"; }} $out/xrdp_keyboard.ini

        cat > $out/startwm.sh <<EOF
        #!/bin/sh
        . /etc/profile
        ${lib.optionalString cfg.audio.enable "${cfg.audio.package}/libexec/pulseaudio-xrdp-module/pulseaudio_xrdp_init"}
        if [ -f ~/startwm.sh ] && [ -x ~/startwm.sh ]; then
          ~/startwm.sh
        else
          ${cfg.defaultWindowManager}
        fi
        EOF
        chmod +x $out/startwm.sh
      '';
  };
  security.pam.services.xrdp-sesman = {
    enableGnomeKeyring = true;
    fprintAuth = false;
  };
}
