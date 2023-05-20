{ pkgs, inputs, system, ... }:
let
  overlay = self: super: {
    libfakeXinerama = super.libfakeXinerama.overrideAttrs (_old: {
      installPhase = ''
        mkdir -p $out/lib
        cp libfakeXinerama.so.1.0 $out/lib
        ln -s libfakeXinerama.so.1.0 $out/lib/libXinerama.so.1.0
        ln -s libfakeXinerama.so.1.0 $out/lib/libfakeXinerama.so.1
        ln -s libXinerama.so.1.0 $out/lib/libXinerama.so.1
        ln -s libXinerama.so.1 $out/lib/libXinerama.so
      '';
    });
  };
  xpraPkgs = import inputs.nixos-unstable {
    overlays = [ overlay ];
    inherit (pkgs) system;
  };
  inherit (xpraPkgs) xpra;
in {
  environment.systemPackages = [ xpra ];
  users = {
    users.bitburner = {
      isSystemUser = true;
      group = "bitburner";
      home = "/var/lib/bitburner";
      createHome = true;
      description = "BitBurner daemon user";
    };
    groups.bitburner = { };
  };
  systemd.services.bitburner = {
    path = [ pkgs.chromium xpra ];
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    script = let
      bitburnerSrc = pkgs.fetchzip {
        url =
          "https://github.com/bitburner-official/bitburner-src/releases/download/v2.2.2/Release2.2.2.zip";
        stripRoot = false;
        hash = "sha256-tXchK+emZKNhjIyrNI9g0oUymRVKt5XTkSJYh6BFsNc=";
      };
    in ''
      xpra start :99 --bind-tcp=10.6.0.6:9292 --bind-tcp=127.0.0.1:9292 --daemon=no --notifications=no --printing=no --speaker=no --session-name=bitburner --exit-with-children --start-child="chromium --app=file://${bitburnerSrc}/index.html --user-data-dir=/var/lib/bitburner/.bitburner-profile"
    '';
    serviceConfig = {
      User = "bitburner";
      RestartSec = 60;
      Restart = "always";
      MemoryHigh = "2G";
      MemoryMax = "4G";
    };
  };
}
