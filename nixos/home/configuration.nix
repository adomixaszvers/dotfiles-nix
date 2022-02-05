# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    # ./bumblebee.nix
    # ./gnome.nix
    # ./kde.nix
    ../aarch64.nix
    ../avahi.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ../zerotier.nix
    ./bumblebee-nvidia.nix
    ./hardware-configuration.nix
    ./remote-build.nix
    ./static-ip.nix
    ./steam.nix
    ./wakeonlan.nix
    ./wireguard-client.nix
  ];

  boot = {
    blacklistedKernelModules = [ "ath9k" ];
    cleanTmpDir = true;
    kernelParams = [ "nohibernate" ];
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      systemd-boot.enable = true;
      grub = {
        enable = false;
        device = "/dev/sda";
        efiSupport = true;
        useOSProber = true;
      };
    };
    supportedFilesystems = [ "zfs" ];
    zfs = {
      forceImportAll = false;
      forceImportRoot = false;
      requestEncryptionCredentials = false;
    };
  };

  environment.systemPackages = with pkgs; [ nixfmt virtmanager ];

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/423B-2D62";
    options = [ "noauto" "x-systemd.automount" ];
    fsType = "vfat";
  };

  fileSystems."/kiti/media" = {
    device = "/dev/disk/by-uuid/AE38E35B38E32157";
    fsType = "ntfs";
    options = [ "noauto" "x-systemd.automount" ];
  };

  networking.domain = "lan";
  networking.hostName = "adomo-nixos"; # Define your hostname.
  networking.hostId = "6665bed8";

  programs.adb.enable = true;
  programs.bash.enableCompletion = true;
  programs.mosh.enable = true;
  programs.ssh.startAgent = false;
  programs.sway.enable = true;
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  services.journald.extraConfig = "SystemMaxUse=500M";
  services.atd.enable = true;
  services.fstrim.enable = true;
  services.xserver.deviceSection = ''
    option "tearfree" "true"
  '';
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  services.teamviewer.enable = false;
  services.thermald.enable = true;

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  hardware.cpu.intel.updateMicrocode = true;
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };
  hardware.pulseaudio.support32Bit = true;

  sops.secrets."adomas/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };
  sops.secrets."root/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };

  users.users.adomas = {
    openssh.authorizedKeys.keyFiles =
      [ ../keys/juice_rsa.pub ../keys/yubikey.pub ];
    extraGroups = [ "docker" "libvirtd" "adbusers" ];
    passwordFile = config.sops.secrets."adomas/password".path;
  };
  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  services.xserver.libinput.enable = true;

  virtualisation = {
    docker.enable = false;
    libvirtd.enable = true;
    virtualbox.host.enable = false;
  };

  system.stateVersion = "21.11"; # Did you read the comment?

}
