{ config, inputs, ... }:
{
  imports = [
    inputs.buildbot-nix.nixosModules.buildbot-worker
  ];
  sops.secrets."buildbot-nix/worker-password" = {
    owner = config.users.users.buildbot-worker.name;
    sopsFile = ../common-secrets/buildbot-nix.yaml;
  };
  services.buildbot-nix.worker = {
    enable = true;
    name = "darbas";
    workerPasswordFile = config.sops.secrets."buildbot-nix/worker-password".path;
    masterUrl = "tcp:host=10.6.0.1:port=9989";
    # The number of workers to start (default: 0 == the number of CPU cores).
    # If you experience flaky builds under high load, try to reduce this value.
    workers = 4;
  };
}
