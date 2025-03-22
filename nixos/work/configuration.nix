# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
{
  imports = [
    ../avahi.nix
    ../common.nix
    ../flakes.nix
    ../gc.nix
    # ../ld-link.nix
    ../libvirtd.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ../thinkfan.nix
    # ./bitburner.nix
    # ./dante.nix
    ./hardware-configuration.nix
    ./kerberos.nix
    ./ltXkb.nix
    ./prebuild-configs.nix
    ./samba.nix
    ./xrdp/current.nix
    # ./turbovnc.nix
    # ./unbound.nix
    # ./vnc.nix
    ../xdg-portal.nix
    # ./wireguard-client.nix
    ../fprintd.nix
    # ./throttled
    ./tlp.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    tmp.cleanOnBoot = true;
    kernelParams = [ "consoleblank=60" ];
    # kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems = [ "zfs" ];
    loader.systemd-boot = {
      enable = true;
      consoleMode = "auto";
    };
    loader.efi.canTouchEfiVariables = true;
    # leave 3 GB free for avoiding systemd-oomd
    extraModprobeConfig = ''
      options zfs zfs_arc_sys_free=${toString (3 * 1024 * 1024 * 1024)}
    '';
  };

  nixpkgs.overlays = [
    (_: super: {
      networkmanager-vpnc = super.networkmanager-vpnc.overrideAttrs (old: {
        patches = old.patches ++ [
          ../../export_nm_vpn_editor_factory_vpnc.patch
        ];
      });
    })
  ];

  networking = {
    enableIPv6 = true;
    hostId = "d864861a";
    hostName = "adomas-jatuzis-nixos"; # Define your hostname.
    domain = "x.insoft.lt";
    dhcpcd.enable = false;
    networkmanager = {
      enable = true;
      dispatcherScripts = [
        {
          type = "basic";
          source =
            pkgs.writeText "wifi-wired-exclusive" # bash
              ''
                export LC_ALL=C
                PATH=${lib.makeBinPath [ pkgs.networkmanager ]}:$PATH

                enable_disable_wifi ()
                {
                    result=$(nmcli dev | grep "ethernet" | grep -w "connected")
                    if [ -n "$result" ]; then
                        nmcli radio wifi off
                    else
                        nmcli radio wifi on
                    fi
                }

                if [ "$2" = "up" ]; then
                    enable_disable_wifi
                fi

                if [ "$2" = "down" ]; then
                    enable_disable_wifi
                fi
              '';
        }
      ];
    };
  };

  security = {
    pki.certificateFiles = [ ./insoft-ca.crt ];
    pam = {
      u2f.enable = true;
      services = {
        hyprlock = {
          fprintAuth = false;
        };
        swaylock = { };
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      wget
      vim
    ];
  };

  programs = {
    # adb.enable = true;
    nix-ld.enable = true;
    ssh = {
      extraConfig = ''
        Host github
          HostName github.com
          ProxyCommand nc -x 10.6.0.1:1080 %h %p
      '';
    };
    mosh.enable = true;
    virt-manager.enable = true;
    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };
  };
  services = {
    autorandr.enable = true;
    avahi.allowInterfaces = [
      "enp0s13f0u3u1"
      "wlp9s0"
    ];
    gnome.glib-networking.enable = true;
    gvfs.enable = true;
    flatpak = {
      enable = true;
    };
    fstrim = {
      enable = true;
      interval = "monthly";
    };
    logind.lidSwitchExternalPower = "ignore";
    openssh = {
      enable = true;
      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
      settings = {
        PasswordAuthentication = false;
        X11Forwarding = true;
      };
      ports = [ 22 ];
    };
    printing = {
      enable = true;
      browsed.enable = false;
    };
    thinkfan.levels = [
      [
        0
        0
        56
      ]
      [
        1
        55
        66
      ]
      [
        2
        65
        71
      ]
      [
        3
        70
        76
      ]
      [
        4
        75
        81
      ]
      [
        5
        80
        86
      ]
      [
        7
        85
        32767
      ]
    ];
    vnstat.enable = true;
  };

  systemd.services.check-internet = {
    unitConfig.FailureAction = "reboot";
    description = "Check if internet connection is still working";
    script = "${pkgs.inetutils}/bin/ping -c5 google.lt";
    startAt = "Mon-Fri 02:00";
  };

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    # listenOptions = [ "/var/run/docker.sock" "2375" ];
  };
  virtualisation.spiceUSBRedirection.enable = true;
  # virtualisation.podman = {
  #   enable = true;
  #   dockerCompat = true;
  #   dockerSocket.enable = true;
  # };
  # virtualisation.containers.storage.settings = {
  #   storage = {
  #     driver = "zfs";
  #     graphroot = "/var/lib/containers/storage";
  #     runroot = "/run/containers/storage";
  #     options.zfs = {
  #       fsname = "rpool/podman";
  #       mountopt = "nodev";
  #     };
  #   };
  # };

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    pulseaudio.support32Bit = true;
    usb-modeswitch.enable = true;
    acpilight.enable = true;
    bluetooth.enable = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "24.05"; # Did you read the comment?

  sops.secrets = {
    "adomas/password" = {
      sopsFile = ./secrets/passwords.yaml;
      neededForUsers = true;
    };
    "root/password" = {
      sopsFile = ./secrets/passwords.yaml;
      neededForUsers = true;
    };
  };

  users = {
    extraGroups.vboxusers.members = [ "adomas" ];
    users = {
      adomas = {
        hashedPasswordFile = config.sops.secrets."adomas/password".path;
        extraGroups = [
          "docker"
          "audio"
          "adbusers"
          "libvirtd"
          "qemu-libvirtd"
          "podman"
          "wireshark"
        ];
        openssh.authorizedKeys.keyFiles = [
          ../keys/laptop.pub
          ../keys/juice_ed25519.pub
          ../keys/yubikey.pub
          ../keys/t14.pub
        ];
      };
      root.hashedPasswordFile = config.sops.secrets."root/password".path;
    };
  };

}
