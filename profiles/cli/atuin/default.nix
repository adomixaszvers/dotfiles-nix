{
  lib,
  inputs,
  config,
  ...
}:
{
  imports = [ inputs.sops-nix.homeManagerModules.sops ];
  programs.atuin = {
    enable = true;
    daemon.enable = true;
    flags = [ "--disable-up-arrow" ];
    settings = {
      auto_sync = true;
      sync_frequency = "1h";
      update_check = false;
      key_path = config.sops.secrets."atuin/key".path;
      session_path = config.sops.secrets."atuin/session".path;
      sync_address = lib.mkDefault "https://atuin.lan.beastade.top";
      search_mode = "fuzzy";
      filter_mode = "host";
      workspaces = true;
      style = "auto";
    };
  };
  sops = {
    age.keyFile = "${config.xdg.configHome}/sops/age/keys.txt";
    secrets =
      let
        sopsFile = ./secrets.yaml;
      in
      {
        "atuin/key" = {
          inherit sopsFile;
        };
        "atuin/session" = {
          inherit sopsFile;
        };
      };
  };
  systemd.user.services.atuin-daemon.Unit.After = [ "sops-nix.service" ];
}
