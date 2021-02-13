{ pkgs, ... }: {
  services.wakeonlan.interfaces = [{
    interface = "enp5s0";
    method = "magicpacket";
  }];
  services.cron.enable = true;
  services.cron.systemCronJobs =
    [ "@reboot root ${pkgs.ethtool}/sbin/ethtool -s enp5s0 wol g" ];
}
