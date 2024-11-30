# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  config,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    # ./bumblebee.nix
    # ./gnome.nix
    # ./kde.nix
    ../avahi.nix
    ../common.nix
    ../flakes.nix
    ../gc.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ./bumblebee-nvidia.nix
    ./hardware-configuration.nix
    # ./remote-build.nix
    ./static-ip.nix
    ./steam.nix
    ./wakeonlan.nix
    ./wireguard-client.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    blacklistedKernelModules = [ "ath9k" ];
    tmp.cleanOnBoot = true;
    kernelParams = [ "nohibernate" ];
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      systemd-boot.enable = true;
    };
    supportedFilesystems = [ "zfs" ];
    zfs = {
      forceImportAll = false;
      forceImportRoot = false;
      requestEncryptionCredentials = false;
    };
  };
  # it fails on zfs
  systemd.generators = {
    systemd-gpt-auto-generator = "/dev/null";
  };

  environment = {
    systemPackages = with pkgs; [ virt-manager ];
    variables.LIBVA_DRIVER_NAME = "i965";
  };

  fileSystems = {
    "/boot/efi" = {
      device = "/dev/disk/by-uuid/423B-2D62";
      options = [
        "noauto"
        "x-systemd.automount"
      ];
      fsType = "vfat";
    };
    "/kiti/media" = {
      device = "/dev/disk/by-uuid/AE38E35B38E32157";
      fsType = "ntfs";
      options = [
        "noauto"
        "x-systemd.automount"
      ];
    };
  };

  networking = {
    domain = "lan";
    hostName = "adomo-nixos"; # Define your hostname.
    hostId = "6665bed8";
  };

  programs = {
    adb.enable = true;
    bash.completion.enable = true;
    mosh.enable = true;
    ssh.startAgent = false;
    sway.enable = true;
  };
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };
  services = {
    autorandr = {
      enable = true;
      defaultTarget = "home-prime";
    };
    flatpak.enable = true;
    journald.extraConfig = "SystemMaxUse=500M";
    atd.enable = true;
    fstrim.enable = true;
    xserver.deviceSection = ''
      option "tearfree" "true"
    '';
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    thermald.enable = true;

    zfs = {
      autoScrub = {
        enable = true;
        interval = "monthly";
      };
      trim.enable = true;
    };
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    pulseaudio.support32Bit = true;
  };

  sops.secrets."adomas/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };
  sops.secrets."root/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };

  users.users.adomas = {
    openssh.authorizedKeys.keyFiles = [
      ../keys/juice_ed25519.pub
      ../keys/yubikey.pub
    ];
    extraGroups = [
      "docker"
      "libvirtd"
      "adbusers"
    ];
    hashedPasswordFile = config.sops.secrets."adomas/password".path;
  };
  users.users.root.hashedPasswordFile = config.sops.secrets."root/password".path;

  virtualisation = {
    libvirtd.enable = true;
  };

  system.stateVersion = "24.05"; # Did you read the comment?

}
