# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }: {
  imports = [
    ../avahi.nix
    ../common.nix
    ../flakes.nix
    ../fprintd.nix
    ../gc.nix
    ../libvirtd.nix
    # ../mt7921e-crash-fix.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ../xdg-portal.nix
    ../zerotier.nix
    ./hardware-configuration.nix
    ./powermanagement.nix
    ./steam.nix
    ./wireguard-client.nix
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen2
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    cleanTmpDir = true;
    kernelParams = [ "nohibernate" ];
    kernelPackages = pkgs.zfs.latestCompatibleLinuxPackages;
    loader = {
      efi = { canTouchEfiVariables = true; };
      systemd-boot.enable = true;
    };
    supportedFilesystems = [ "zfs" ];
    zfs = { requestEncryptionCredentials = false; };
  };
  # it fails on zfs
  systemd.generators = { systemd-gpt-auto-generator = "/dev/null"; };
  hardware.bluetooth.enable = true;
  powerManagement.resumeCommands = ''
    /run/current-system/sw/bin/bluetoothctl discoverable on
  '';
  hardware.video.hidpi.enable = true;
  hardware.xone.enable = true;

  environment.systemPackages = with pkgs; [ nixfmt virtmanager ];

  networking.domain = "lan";
  networking.hostName = "adomo-t14"; # Define your hostname.
  networking.hostId = "81046d10";

  programs.adb.enable = true;
  programs.bash.enableCompletion = true;
  programs.mosh.enable = true;
  programs.ssh.startAgent = false;
  services.flatpak.enable = true;
  services.autorandr = {
    enable = true;
    defaultTarget = "home-prime";
  };
  services.journald.extraConfig = "SystemMaxUse=500M";
  services.atd.enable = true;
  services.fstrim.enable = true;
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

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

  system.stateVersion = "22.05"; # Did you read the comment?

}
