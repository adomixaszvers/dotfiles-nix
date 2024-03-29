{ config, pkgs, inputs, ... }: {
  environment.etc = {
    "xrdp/sesman.ini".source = "${config.services.xrdp.confDir}/sesman.ini";
  };
  services.xrdp = {
    enable = true;
    confDir = let
      inherit (inputs) self;
      cfg = config.services.xrdp;
    in pkgs.runCommand "xrdp.conf" { preferLocalBuild = true; } ''
      mkdir $out

      cp ${pkgs.xrdp}/etc/xrdp/{km-*,xrdp,sesman}.ini $out
      cp ${
        builtins.path { path = "${self}/pkgs/custom-xrdp/km-00010427.ini"; }
      } $out
      cp ${
        builtins.path { path = "${self}/pkgs/custom-xrdp/xrdp_keyboard.ini"; }
      } $out

      cat > $out/startwm.sh <<EOF
      #!/bin/sh
      . /etc/profile
      ${cfg.defaultWindowManager}
      EOF
      chmod +x $out/startwm.sh

      substituteInPlace $out/xrdp.ini \
        --replace "#rsakeys_ini=" "rsakeys_ini=/run/xrdp/rsakeys.ini" \
        --replace "certificate=" "certificate=${cfg.sslCert}" \
        --replace "key_file=" "key_file=${cfg.sslKey}" \
        --replace LogFile=xrdp.log LogFile=/dev/null \
        --replace EnableSyslog=true EnableSyslog=false

      substituteInPlace $out/sesman.ini \
        --replace LogFile=xrdp-sesman.log LogFile=/dev/null \
        --replace EnableSyslog=true EnableSyslog=true \
        --replace 'FuseMountName=thinclient_drives' '#FuseMountName=thinclient_drives' \
        --replace '#FuseMountName=/run/user/%u/thinclient_drives' 'FuseMountName=/run/user/%u/thinclient_drives'

      # Ensure that clipboard works for non-ASCII characters
      sed -i -e '/.*SessionVariables.*/ a\
      LANG=${config.i18n.defaultLocale}\
      LOCALE_ARCHIVE=${config.i18n.glibcLocales}/lib/locale/locale-archive
      ' $out/sesman.ini
    '';
  };
  security.pam.services.xrdp-sesman = {
    enableGnomeKeyring = true;
    fprintAuth = false;
  };
}
