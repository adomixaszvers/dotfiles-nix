{ pkgs, ... }:
let
  inherit (pkgs) xpra;
in
{
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
    path = [
      pkgs.chromium
      xpra
    ];
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    script =
      let
        version = "2.6.2";
        bitburnerSrc = pkgs.fetchzip {
          url = "https://github.com/bitburner-official/bitburner-src/releases/download/v${version}/${
            builtins.replaceStrings [ "." ] [ "_" ] version
          }_Web.zip";
          stripRoot = false;
          sha256 = "11pkmxsbmihz3skl29qx1s51qabwil9ayzkf3y5zv6dq7mairldn";
        };
      in
      ''
        xpra start :99 --bind-tcp=10.6.0.6:9292 --bind-tcp=127.0.0.1:9292 --daemon=no --notifications=no --printing=no --speaker=no --session-name=bitburner --exit-with-children --start-child="chromium --app=file://${bitburnerSrc}/Bitburner\ ${version}/index.html --user-data-dir=/var/lib/bitburner/.bitburner-profile"
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
