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
    ../thinkfan.nix
    ./hardware-configuration.nix
    ./powermanagement.nix
    ./steam.nix
    ./wireguard-client.nix
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t14-amd-gen2
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    tmp.cleanOnBoot = true;
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
  powerManagement.resumeCommands = ''
    /run/current-system/sw/bin/bluetoothctl discoverable on
  '';
  console.font = "${pkgs.terminus_font}/share/consolefonts/ter-v32n.psf.gz";
  hardware = {
    bluetooth.enable = true;
    trackpoint.emulateWheel = false;
    xone.enable = true;
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
    pulseaudio.support32Bit = true;
  };

  environment.systemPackages = with pkgs; [ nixfmt virtmanager ];

  networking = {
    domain = "lan";
    hostName = "adomo-t14"; # Define your hostname.
    hostId = "81046d10";
  };

  programs = {
    adb.enable = true;
    bash.enableCompletion = true;
    mosh.enable = true;
    ssh.startAgent = false;
  };
  services = {
    flatpak.enable = true;
    autorandr = {
      enable = true;
      defaultTarget = "home-prime";
    };
    journald.extraConfig = "SystemMaxUse=500M";
    atd.enable = true;
    fstrim.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };

    zfs = {
      autoScrub = {
        enable = true;
        interval = "monthly";
      };
      trim.enable = true;
    };
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
    openssh.authorizedKeys.keyFiles =
      [ ../keys/juice_ed25519.pub ../keys/yubikey.pub ];
    extraGroups = [ "docker" "libvirtd" "adbusers" ];
    passwordFile = config.sops.secrets."adomas/password".path;
  };
  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  services.xserver.libinput.enable = true;

  system.stateVersion = "22.05"; # Did you read the comment?

}
