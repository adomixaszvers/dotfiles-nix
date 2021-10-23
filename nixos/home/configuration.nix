# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./bumblebee.nix
    # ./nvidia.nix
    ./hardware-configuration.nix
    ./static-ip.nix
    ./wakeonlan.nix
    "${inputs.credentials}/home-secrets.nix"
  ];

  # boot.kernelParams = [ "scsi_mod.use_blk_mq=1" "dm_mod.use_blk_mq=y" ];
  boot = {
    blacklistedKernelModules = [ "ath9k" ];
    cleanTmpDir = true;
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

  networking.hostName = "adomo-nixos"; # Define your hostname.
  networking.hostId = "6665bed8";

  programs.adb.enable = true;
  programs.bash.enableCompletion = true;
  programs.mosh.enable = true;
  programs.ssh.startAgent = true;
  services.avahi = {
    enable = true;
    interfaces = [ "enp5s0" ];
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
    };
  };
  services.flatpak.enable = true;
  xdg.portal.enable = true;
  services.journald.extraConfig = "SystemMaxUse=500M";
  services.kmscon = {
    enable = false;
    extraConfig = ''
      font-name=FiraMono Nerd Font
      font-size=9
    '';
    hwRender = true;
  };
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
  # services.udev.extrarules = ''
  #   action=="add|change", kernel=="[sv]d[a-z]", attr{queue/rotational}=="1", attr{queue/scheduler}="bfq"
  #   action=="add|change", kernel=="[sv]d[a-z]", attr{queue/rotational}=="0", attr{queue/scheduler}="bfq"
  # '';

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

  users.users.adomas = {
    openssh.authorizedKeys.keyFiles = [ ./juice_rsa.pub ./yubikey.pub ];
    extraGroups = [ "docker" "libvirtd" "adbusers" ];
  };

  services.xserver.libinput.enable = true;

  virtualisation = {
    docker.enable = false;
    libvirtd.enable = true;
    virtualbox.host.enable = false;
  };

  system.stateVersion = "20.09"; # Did you read the comment?

}
