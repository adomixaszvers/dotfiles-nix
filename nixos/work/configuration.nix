# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }@args:
let inputs = if args ? inputs then args.inputs else import ../../inputs.nix;
in {
  _module.args.inputs = inputs;
  imports = [
    ../avahi.nix
    ../common.nix
    ../flakes.nix
    ../gc.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ../zerotier.nix
    ./hardware-configuration.nix
    ./iperf3.nix
    ./ltXkb.nix
    ./prebuild-configs.nix
    ./samba.nix
    ./turbovnc.nix
    # ./unbound.nix
    ./vnc.nix
    ./wireguard-client.nix
    ./fprintd.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  # Use the systemd-boot EFI boot loader.
  boot.cleanTmpDir = true;
  boot.kernelParams = [ "consoleblank=60" ];
  boot.kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
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
    dhcpcd.enable = false;
    networkmanager.enable = true;
  };

  environment.systemPackages = with pkgs; [ wget vim virtmanager ];
  environment.variables = {
    VK_ICD_FILENAMES =
      "/run/opengl-driver/share/vulkan/icd.d/intel_icd.x86_64.json";
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.adb.enable = true;
  programs.mosh.enable = true;
  programs.steam.enable = true;
  programs.sway.enable = true;
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };
  services.xserver.libinput.enable = true;

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
  services.dante = {
    enable = false;
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
  services.thermald.enable = true;
  services.vnstat.enable = true;

  services.xrdp.enable = true;
  services.xrdp.package = inputs.self.packages.x86_64-linux.custom-xrdp;
  networking.firewall.interfaces.ztzlgoe57z.allowedTCPPorts = [ 3389 ];

  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;

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
    [ ../keys/laptop.pub ../keys/juice_ed25519.pub ../keys/yubikey.pub ];

}
