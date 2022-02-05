{
  services.fail2ban = {
    enable = true;
    ignoreIP = [ "192.168.0.0/16" "10.6.0.0/24" "10.147.17.0/24" ];
    jails = {
      sshd = ''
        enabled   = true
        filter    = sshd
        banaction = iptables
        backend   = systemd
        maxretry  = 5
        findtime  = 1d
        bantime   = 2w
      '';
    };
  };
}
