{ pkgs, config, ... }:
{
  sops = {
    secrets."forgejo-runner/token".sopsFile = ./secrets/forgejo-runner.yaml;
  };
  services.forgejo-runner = {
    instances.default = {
      enable = true;
      settings = {
        container.options = "--add-host git.bl.beastade.top:host-gateway";
        server.connections = {
          default = {
            url = "https://git.bl.beastade.top";
            uuid = "a7e04e3a-b25c-45b2-9edd-f299c9d9621f";
          };
        };
        runner.labels = [
          "ubuntu-24.04:docker://gitea/runner-images:ubuntu-latest"
          "ubuntu-slim:docker://gitea/runner-images:ubuntu-latest-slim"
          "native:host"
        ];
      };
      secrets.server.connections.default = {
        token_url = config.sops.secrets."forgejo-runner/token".path;
      };
      hostPackages = [
        config.nix.package
      ]
      ++ (builtins.attrValues {
        inherit (pkgs)
          # keep-sorted start
          bash
          coreutils
          curl
          gawk
          gitMinimal
          gnused
          jq
          nodejs
          wget
          # keep-sorted end
          ;
      });
    };
  };
  systemd.services.gitea-runner-default.unitConfig.After = [ "forgejo.service" ];
}
