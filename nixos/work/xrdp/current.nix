{ config, inputs, ... }: {
  environment.etc = {
    "xrdp/sesman.ini".source = "${config.services.xrdp.confDir}/sesman.ini";
  };
  services.xrdp = {
    enable = true;
    extraConfDirCommands = let
      inherit (inputs) self;
      cfg = config.services.xrdp;
      # bash
    in ''
      cp ${
        builtins.path { path = "${self}/pkgs/custom-xrdp/km-00010427.ini"; }
      } $out/km-00010427.ini
      cp ${
        builtins.path { path = "${self}/pkgs/custom-xrdp/xrdp_keyboard.ini"; }
      } $out/xrdp_keyboard.ini

      cat > $out/startwm.sh <<EOF
      #!/bin/sh
      . /etc/profile
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
