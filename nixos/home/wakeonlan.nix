{ pkgs, ... }: {
  networking.interfaces.enp5s0.wakeOnLan.enable = true;
  services.cron.enable = true;
  services.cron.systemCronJobs =
    [ "@reboot root ${pkgs.ethtool}/sbin/ethtool -s enp5s0 wol g" ];
}
