{ pkgs, ... }: {
  environment.systemPackages = [ pkgs.xpra ];
  systemd.services.bitburner = {
    path = with pkgs; [ xpra chromium ];
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    script = let
      bitburnerSrc = pkgs.fetchzip {
        url =
          "https://github.com/bitburner-official/bitburner-src/releases/download/v2.2.1/Bitburner.v2.2.1.zip";
        stripRoot = false;
        hash = "sha256-IGu4/GHHfE/d1OrG2d8Y5XDDTAlrSn/xbc1ory7mtAg=";
      };
    in ''
      xpra start --printing=no --speaker=no --session-name=bitburner --no-daemon --exit-with-children --start-child="chromium --app=file://${bitburnerSrc}/index.html --user-data-dir=/home/adomas/.bitburner-profile"
    '';
    serviceConfig = { User = "adomas"; };
  };
}
