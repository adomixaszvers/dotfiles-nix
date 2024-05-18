{
  # ported from https://gitlab.archlinux.org/archlinux/packaging/packages/realtime-privileges
  users = {
    users.adomas.extraGroups = [ "realtime" ];
    groups.realtime = { };
  };
  services.udev.extraRules = # udev
    ''
      KERNEL=="cpu_dma_latency", GROUP="realtime"
    '';
  security.pam.loginLimits = [
    {
      domain = "@realtime";
      type = "-";
      item = "rtprio";
      value = 98;
    }
    {
      domain = "@realtime";
      type = "-";
      item = "memlock";
      value = "unlimited";
    }
    {
      domain = "@realtime";
      type = "-";
      item = "nice";
      value = -11;
    }
  ];
}
