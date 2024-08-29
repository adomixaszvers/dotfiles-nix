{ config, ... }:
{
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
    forwardPorts = [
      {
        containerPort = 1080;
        hostPort = 1090;
        protocol = "tcp";
      }
    ];
    bindMounts.vpnc = {
      hostPath = "/root/darbas-vpnc.conf";
      isReadOnly = true;
      mountPoint = "/vpnc.conf";
    };
    privateNetwork = true;
    hostAddress = "192.168.100.10";
    localAddress = "192.168.100.11";
    ephemeral = true;
    config =
      { pkgs, ... }:
      {
        environment.etc."resolv.conf".text = ''
          search x.insoft.lt
          nameserver 192.168.30.11
          nameserver 192.168.30.12
          nameserver 192.168.30.1
          nameserver 9.9.9.9
          options edns0
        '';
        environment.systemPackages = [ pkgs.kitty.terminfo ];
        networking = {
          extraHosts = ''
            88.119.198.57 vpn.insoft.lt
          '';
          firewall.allowedTCPPorts = [ 1080 ];
          interfaces.eth0.ipv4.routes = [
            {
              address = "192.168.1.0";
              prefixLength = 24;
              via = "192.168.100.10";
            }
          ];
        };
        systemd.services.vpnc = {
          enable = true;
          after = [ "network-online.target" ];
          wants = [ "network-online.target" ];
          requires = [ "network.target" ];
          wantedBy = [ "multi-user.target" ];
          serviceConfig = {
            Restart = "on-failure";
            RestartSec = 30;
          };
          script = "${pkgs.vpnc}/bin/vpnc /vpnc.conf --no-detach --non-inter --local-port 0";
        };
        systemd.services.dante.requires = [ "vpnc.service" ];
        services.dante = {
          enable = true;
          config = ''
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
