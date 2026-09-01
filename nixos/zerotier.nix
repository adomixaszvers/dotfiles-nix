{ lib, config, ... }: {
  networking.hosts = {
    "10.147.17.214" = [ "work.zt" ];
    "10.147.17.222" = [ "t14.zt" ];
    "10.147.17.171" = [ "pc.zt" ];
  };
  sops.secrets."zerotier/networks" = {
    sopsFile = ./common-secrets/zerotier.yaml;
  };
  services.zerotierone = {
    enable = true;
  };
  systemd.services.zerotierone.preStart = lib.mkAfter ''
    for networkId in $(<${config.sops.secrets."zerotier/networks".path}); do
      touch "/var/lib/zerotier-one/networks.d/''${networkId}.conf"
    done
  '';
}
