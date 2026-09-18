{ config, pkgs, ... }:
{
  sops.secrets =
    let
      sopsConfig = {
        sopsFile = ./secrets/renovate.yaml;
      };
    in
    {
      renovateGithubToken = sopsConfig;
      renovateToken = sopsConfig;
    };
  services.renovate = {
    enable = true;
    credentials = {
      GITHUB_COM_TOKEN = config.sops.secrets.renovateGithubToken.path;
      RENOVATE_TOKEN = config.sops.secrets.renovateToken.path;
    };
    runtimePackages = [
      config.nix.package
      pkgs.openssh
    ];
    settings = {
      endpoint = "https://git.bl.beastade.top";
      gitAuthor = "Renovate <renovate@beelink>";
      platform = "forgejo";
      autodiscover = false;
      configMigration = true;
      extends = [
        "config:recommended"
        ":dependencyDashboard"
        "helpers:pinGitHubActionDigests"
      ];
      lockFileMaintenance = {
        enabled = true;
        schedule = [ "at any time" ];
      };
      nix.enabled = true;
      onboardingConfigFileName = "renovate.json";
      optimizeForDisabled = true;
      osvVulnerabilityAlerts = true;
      packageRules = [
        {
          groupName = "flake inputs";
          matchManagers = [ "nix" ];
        }
      ];
      persistRepoData = true;
      prConcurrentLimit = 0;
      prHourlyLimit = 0;
      repositories = [ "adomas/dotfiles-nix" ];
    };
    schedule = "2:00";
    validateSettings = true;
  };
}
