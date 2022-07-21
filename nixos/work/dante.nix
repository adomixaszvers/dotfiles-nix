{
  networking.firewall.interfaces.wg0.allowedTCPPorts = [ 1080 ];
  services.dante = {
    enable = true;
    config = ''
      internal: 10.6.0.6 port = 1080
      external: enp0s13f0u3u1

      clientmethod: none
      socksmethod: none

      client pass {
        from: 10.6.0.0/24 to: 0.0.0.0/0
        log: error # connect disconnect
      }

      #generic pass statement - bind/outgoing traffic
      socks pass {
              from: 0.0.0.0/0 to: 0.0.0.0/0
              command: bind connect udpassociate
              log: error # connect disconnect iooperation
      }

      #generic pass statement for incoming connections/packets
      socks pass {
              from: 0.0.0.0/0 to: 0.0.0.0/0
              command: bindreply udpreply
              log: error # connect disconnect iooperation
      }
    '';
  };
}
