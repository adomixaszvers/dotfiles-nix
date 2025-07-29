{ config, lib, ... }:
{
  sops.secrets =
    let
      secretArgs = {
        sopsFile = ./common-secrets/atuin.yaml;
        owner = lib.mkDefault config.users.users.adomas.name;
      };
    in
    {
      "atuin/key" = secretArgs;
      "atuin/session" = secretArgs;
    };
}
