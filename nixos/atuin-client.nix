{ config, lib, ... }:
{
  sops.secrets."atuin/key" = {
    sopsFile = ./common-secrets/atuin.yaml;
    owner = lib.mkDefault config.users.users.adomas.name;
  };
}
