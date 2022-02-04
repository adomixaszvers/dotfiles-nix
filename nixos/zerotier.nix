{ lib, config, pkgs, ... }: {
  sops.secrets."zerotier/networks" = {
    sopsFile = ./common-secrets/zerotier.yaml;
  };
  services.zerotierone = { enable = true; };
  systemd.services.zerotierone.preStart = lib.mkAfter ''
    for networkId in $(<${config.sops.secrets."zerotier/networks".path}); do
      touch "/var/lib/zerotier-one/networks.d/''${networkId}.conf"
    done
  '';
}
