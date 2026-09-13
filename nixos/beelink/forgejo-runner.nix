{ pkgs, config, ... }:
{
  sops = {
    secrets."forgejo-runner/token".sopsFile = ./secrets/forgejo-runner.yaml;
    templates."forgejo-runner.env".content = ''
      TOKEN=${config.sops.placeholder."forgejo-runner/token"}
    '';
  };
  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.default = {
      enable = true;
      name = "monolith";
      url = "https://git.bl.beastade.top";
      # Obtaining the path to the runner token file may differ
      # tokenFile should be in format TOKEN=<secret>, since it's EnvironmentFile for systemd
      settings.container.options = "--add-host git.bl.beastade.top:host-gateway";
      tokenFile = config.sops.templates."forgejo-runner.env".path;
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
      labels = [
        "ubuntu-24.04:docker://gitea/runner-images:ubuntu-latest"
        "ubuntu-slim:docker://gitea/runner-images:ubuntu-latest-slim"
        "native:host"
      ];
    };
  };
  systemd.services.gitea-runner-default.unitConfig.After = [ "forgejo.service" ];
}
