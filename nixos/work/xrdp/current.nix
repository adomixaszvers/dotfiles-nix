{ config, inputs, ... }: {
  environment.etc = {
    "xrdp/sesman.ini".source = "${config.services.xrdp.confDir}/sesman.ini";
  };
  services.xrdp = {
    enable = true;
    extraConfDirCommands = let inherit (inputs) self;
    in ''
      cp ${
        builtins.path { path = "${self}/pkgs/custom-xrdp/km-00010427.ini"; }
      } $out
      cp ${
        builtins.path { path = "${self}/pkgs/custom-xrdp/xrdp_keyboard.ini"; }
      } $out
    '';
  };
  security.pam.services.xrdp-sesman = {
    enableGnomeKeyring = true;
    fprintAuth = false;
  };
}
