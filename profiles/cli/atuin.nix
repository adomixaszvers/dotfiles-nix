{ lib, ... }:
{
  programs.atuin = {
    enable = true;
    settings = {
      auto_sync = true;
      sync_frequency = "5m";
      key_path = "/run/secrets/atuin/key";
      sync_address = lib.mkDefault "https://atuin.lan.beastade.top";
      search_mode = "prefix";
    };
  };
}
