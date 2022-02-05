# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:

{
  imports = [
    ../avahi.nix
    ../common.nix
    ../flakes.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ../zerotier.nix
    ./gc.nix
    ./hardware-configuration.nix
    ./iperf3.nix
    ./ltXkb.nix
    ./samba.nix
    ./turbovnc.nix
    ./unbound.nix
    ./vnc.nix
    ./wireguard-client.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.cleanTmpDir = true;
  boot.kernelParams = [ "consoleblank=60" ];
  boot.loader.systemd-boot = {
    enable = true;
    consoleMode = "auto";
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "zfs" ];
  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  networking = {
    hostId = "d864861a";
    hostName = "adomas-jatuzis-nixos"; # Define your hostname.
    domain = "x.insoft.lt";
    defaultGateway = "192.168.34.1";
    dhcpcd.enable = false;
    interfaces.eno1.ipv4.addresses = [{
      address = "192.168.33.105";
      prefixLength = 22;
    }];
    networkmanager.enable = true;
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
  };

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [ wget vim virtmanager ];

  # musnix.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.adb.enable = true;
  programs.mosh.enable = true;
  programs.sway.enable = true;
  services.x2goserver = {
    enable = false;
    settings = { log.loglevel = "info"; };
  };
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };

  # List services that you want to enable:

  virtualisation.docker = {
    enable = true;
    storageDriver = "zfs";
    # listenOptions = [ "/var/run/docker.sock" "2375" ];
  };
  virtualisation.libvirtd = {
    enable = true;
    qemu.runAsRoot = false;
  };
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
  virtualisation.virtualbox.host = {
    enable = false;
    # enableExtensionPack = true;
  };
  services.dante = {
    enable = true;
    config = ''
      internal: 10.6.0.6 port = 1080
      external: eno1

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
  services.gvfs.enable = true;
  # security.pam.services.lightdm.enableGnomeKeyring = true;
  services.flatpak = { enable = true; };
  xdg.portal = { enable = true; };
  services.fstrim = {
    enable = false;
    interval = "monthly";
  };
  services.openssh = {
    enable = true;
    forwardX11 = true;
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
    passwordAuthentication = false;
    ports = [ 9222 22 ];
  };
  services.printing.enable = true;
  services.postgresql.enable = false;
  services.rabbitmq = {
    enable = false;
    # plugins = [ "rabbitmq_management" ];
  };
  services.squid.enable = true;
  services.teamviewer.enable = false;
  services.thermald.enable = true;
  services.vnstat.enable = true;
  services.vsftpd = {
    anonymousUser = true;
    anonymousUploadEnable = true;
    enable = true;
  };

  services.xrdp.enable = true;
  services.xrdp.package = inputs.self.packages.x86_64-linux.custom-xrdp;
  # services.xrdp.defaultWindowManager = "${pkgs.icewm}/bin/icewm";
  # networking.firewall.allowedTCPPorts = [ 3389 ];
  networking.firewall.interfaces.ztzlgoe57z.allowedTCPPorts = [ 3389 ];

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.sddm.enable = true;
  # services.xserver.desktopManager.plasma5.enable = true;

  # KDE
  # services.xserver = {
  #   displayManager.lightdm.enable = false;
  #   displayManager.sddm.enable = true;
  #   desktopManager.plasma5.enable = true;
  # };

  # Gnome
  # services.xserver = {
  #   displayManager.lightdm.enable = false;
  #   displayManager.gdm.enable = true;
  #   desktopManager.gnome3.enable = true;
  # };

  # Pantheon
  # services.xserver = {
  #   displayManager.lightdm.enable = true;
  #   desktopManager.pantheon.enable = true;
  # };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.11"; # Did you read the comment?

  users.extraGroups.vboxusers.members = [ "adomas" ];

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

  users.users.adomas.passwordFile = config.sops.secrets."adomas/password".path;
  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  users.users.adomas.extraGroups = [
    "docker"
    "audio"
    "adbusers"
    "libvirtd"
    "qemu-libvirtd"
    "podman"
    "wireshark"
  ];
  users.users.adomas.openssh.authorizedKeys.keyFiles =
    [ ../keys/laptop.pub ../keys/juice_rsa.pub ../keys/yubikey.pub ];

}
