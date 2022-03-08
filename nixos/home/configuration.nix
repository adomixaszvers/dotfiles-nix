# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }@args:
let inputs = if args ? inputs then args.inputs else import ../../inputs.nix;
in {
  _module.args.inputs = inputs;
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
    ../zerotier.nix
    ./bumblebee-nvidia.nix
    ./hardware-configuration.nix
    ./remote-build.nix
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
    cleanTmpDir = true;
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

  environment.systemPackages = with pkgs; [ nixfmt virtmanager ];
  environment.variables.LIBVA_DRIVER_NAME = "i965";

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
  services.autorandr = {
    enable = true;
    defaultTarget = "home-prime";
  };
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
      [ ../keys/juice_ed25519.pub ../keys/yubikey.pub ];
    extraGroups = [ "docker" "libvirtd" "adbusers" ];
    passwordFile = config.sops.secrets."adomas/password".path;
  };
  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  services.xserver.libinput.enable = true;

  virtualisation = { libvirtd.enable = true; };

  system.stateVersion = "21.11"; # Did you read the comment?

}
