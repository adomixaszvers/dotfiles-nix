{ pkgs, ... }:
let inherit (pkgs) xpra;
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
          "https://github.com/bitburner-official/bitburner-src/releases/download/v2.3.1/bitburner.2.3.1.zip";
        stripRoot = false;
        sha256 = "01cnwqhh64kkfnfy6zyzpg35r775f03irfx1f0cjxq2n3yzsr2p4";
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
