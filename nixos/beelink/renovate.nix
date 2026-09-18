{ config, ... }:
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
    settings = {
      endpoint = "https://git.bl.beastade.top";
      autodiscover = true;
      gitAuthor = "Renovate <renovate@beelink>";
      platform = "forgejo";
    };
    schedule = "*:0/10";
  };
}
