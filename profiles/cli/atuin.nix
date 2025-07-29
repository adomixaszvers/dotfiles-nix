{ lib, ... }:
{
  programs.atuin = {
    enable = true;
    daemon.enable = true;
    settings = {
      auto_sync = true;
      sync_frequency = "1h";
      update_check = false;
      key_path = "/run/secrets/atuin/key";
      session_path = "/run/secrets/atuin/session";
      sync_address = lib.mkDefault "https://atuin.lan.beastade.top";
      search_mode = "fuzzy";
      filter_mode = "host";
      workspaces = true;
      style = "auto";
    };
  };
}
