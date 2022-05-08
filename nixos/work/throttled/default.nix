{ pkgs, ... }: {
  services.throttled = {
    enable = true;
    extraConfig = builtins.readFile ./throttled.conf;
  };
  systemd.services.throttled-helper = {
    description = "Change CPU MCHBAR P1 to 20 watts";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "always";
      ExecStart = pkgs.writers.writeRust "set20watts" { strip = true; }
        (builtins.readFile ./helper.rs);
    };
  };
}
