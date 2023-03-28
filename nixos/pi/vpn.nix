{ config, ... }: {
  sops.secrets."vpnc/config" = {
    sopsFile = ../common-secrets/vpnc.conf;
    format = "binary";
  };
  networking.firewall.interfaces.eth0.allowedTCPPorts = [ 1090 ];
  networking.nat = {
    enable = true;
    internalInterfaces = [ "ve-+" ];
    externalInterface = "eth0";
    # forwardPorts = [{
    #   destination = "192.168.100.11:1080";
    #   sourcePort = 1090;
    # }];
  };
  containers.vpnc = {
    autoStart = true;
    enableTun = true;
    forwardPorts = [{
      containerPort = 1080;
      hostPort = 1090;
      protocol = "tcp";
    }];
    bindMounts.vpnc = {
      hostPath = config.sops.secrets."vpnc/config".path;
      isReadOnly = true;
      mountPoint = "/vpnc.conf";
    };
    privateNetwork = true;
    hostAddress = "192.168.100.10";
    localAddress = "192.168.100.11";
    config = { pkgs, ... }: {
      networking.nameservers = [ "9.9.9.9" ];
      environment.systemPackages = [ pkgs.kitty.terminfo ];
      networking.firewall.allowedTCPPorts = [ 1080 ];
      networking.interfaces.eth0.ipv4.routes = [{
        address = "192.168.1.0";
        prefixLength = 24;
        via = "192.168.100.10";
      }];
      systemd.services.vpnc = {
        enable = true;
        after = [ "network-online.target" ];
        requires = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        script =
          "${pkgs.vpnc}/bin/vpnc /vpnc.conf --no-detach --non-inter --local-port 0";
      };
      systemd.services.dante.requires = [ "vpnc.service" ];
      services.dante = {
        enable = true;
        config = ''
          debug: 1
          internal: 0.0.0.0 port = 1080
          external: tun0

          clientmethod: none
          socksmethod: none

          client pass {
            from: 192.168.100.0/24 to: 0.0.0.0/0
            log: error # connect disconnect
          }

          client pass {
            from: 192.168.1.0/24 to: 0.0.0.0/0
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
      system.stateVersion = "22.11";
    };
  };
}