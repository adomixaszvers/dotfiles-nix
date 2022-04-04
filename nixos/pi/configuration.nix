{ pkgs, lib, ... }@args:
let inputs = if args ? inputs then args.inputs else import ../../inputs.nix;
in {
  _module.args.inputs = inputs;
  imports = [
    # ./vaultwarden.nix
    ../flakes.nix
    ../nix-registry.nix
    ../zerotier.nix
    ./acme.nix
    ./dns.nix
    # ./fail2ban.nix
    ./nginx.nix
    ./static-ip.nix
    ./syncthing.nix
    ./users.nix
    ./prebuild-configs.nix
    # ./webdav.nix
    ./wireguard.nix
    ./zsh.nix
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    inputs.sops-nix.nixosModules.sops
  ];

  nixpkgs.config.allowUnfree = true;

  boot.tmpOnTmpfs = true;
  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
    firmwareConfig = ''
      dtoverlay=disable-wifi
      dtoverlay=disable-bt
    '';
  };

  programs.mosh.enable = true;

  services.udev.extraRules = ''
    SUBSYSTEM=="vchiq",KERNEL=="vchiq",GROUP="video",MODE="0660"
  '';

  # File systems configuration for using the installer's partition layout
  fileSystems = {
    # Prior to 19.09, the boot partition was hosted on the smaller first partition
    # Starting with 19.09, the /boot folder is on the main bigger partition.
    # The following is to be used only with older images.
    /* "/boot" = {
         device = "/dev/disk/by-label/NIXOS_BOOT";
         fsType = "vfat";
       };
    */
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  time.timeZone = "Europe/Vilnius";

  nix = {
    autoOptimiseStore = true;
    binaryCaches = lib.mkAfter [
      "https://pre-commit-hooks.cachix.org"
      "https://nix-community.cachix.org"
      "https://adomixaszvers.cachix.org"
    ];
    binaryCachePublicKeys = [
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "adomixaszvers.cachix.org-1:r3/lrlbDE7o/Vjk/muEU2iLIiCEZMbC09ZqiwAs64so="
    ];
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    # Free up to 1GiB whenever there is less than 100MiB left.
    extraOptions = ''
      keep-outputs = true
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
    '';
  };

  environment.systemPackages = with pkgs; [
    bind
    git
    libraspberrypi
    lm_sensors
    neovim
    nixfmt
    wol
  ];
  networking = { hostName = "raspberrypi-nixos"; };
  services.dante = {
    enable = true;
    config = ''
      internal: 10.6.0.1 port = 1080
      external: eth0

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
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  virtualisation.podman.enable = true;
  virtualisation.oci-containers.backend = "podman";

  powerManagement.cpuFreqGovernor = "schedutil";

  system.stateVersion = "21.11";
}
