{ pkgs, ... }:
{
  services.throttled = {
    enable = true;
    extraConfig = builtins.readFile ./throttled.conf;
  };
  systemd.services.throttled-helper = {
    description = "Change CPU MCHBAR P1 to 20 watts";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "always";
      ExecStart =
        let
          set20watts = pkgs.writers.writeRustBin "set20watts" { strip = true; } (
            builtins.readFile ./helper.rs
          );
        in
        "${set20watts}/bin/set20watts";
    };
  };
}
