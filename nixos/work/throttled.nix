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
      ExecStart = pkgs.writers.writeDash "set20watts.sh" ''
        while true; do
          echo 20000000 >/sys/devices/virtual/powercap/intel-rapl-mmio/intel-rapl-mmio:0/constraint_0_power_limit_uw;
          sleep 10;
        done
      '';
    };
  };
}
