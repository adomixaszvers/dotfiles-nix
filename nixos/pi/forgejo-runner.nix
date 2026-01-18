{ pkgs, config, ... }:
{
  sops = {
    secrets."forgejo-runner/token".sopsFile = ./secrets/forgejo-runner.yaml;
    templates."forgejo-runner.env".content = ''
      TOKEN=${config.sops.placeholder."forgejo-runner/token"}
    '';
  };
  # systemd.services.gitea-actions-runner.serviceConfig = {
  #   LoadCredential = [
  #     "token:${config.sops.secrets."forgejo-runner/token".path}"
  #   ];
  #   Environment = [
  #     "TOKEN=%d/token"
  #   ];
  # };
  services.gitea-actions-runner = {
    package = pkgs.forgejo-runner;
    instances.default = {
      enable = true;
      name = "monolith";
      url = "https://git.rpi4.beastade.top";
      # Obtaining the path to the runner token file may differ
      # tokenFile should be in format TOKEN=<secret>, since it's EnvironmentFile for systemd
      settings.container.options = "--add-host git.rpi4.beastade.top:host-gateway";
      tokenFile = config.sops.templates."forgejo-runner.env".path;
      labels = [
        "ubuntu-24.04-arm:docker://gitea/runner-images:ubuntu-latest"
        # "ubuntu-latest:docker://node:16-bullseye"
        # "ubuntu-22.04:docker://node:16-bullseye"
        # "ubuntu-20.04:docker://node:16-bullseye"
        # "ubuntu-18.04:docker://node:16-buster"
        ## optionally provide native execution on the host:
        # "native:host"
      ];
    };
  };
}
