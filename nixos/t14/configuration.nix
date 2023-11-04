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
    ../nix-registry.nix
    ../pipewire.nix
    ../realtime.nix
    ../syncthing.nix
    ../yubikey.nix
    # ../xdg-portal.nix
    ../thinkfan.nix
    ./hardware-configuration.nix
    # ./powermanagement.nix
    ../steam.nix
    ./wireguard-client.nix
    ../kde.nix
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

  environment.systemPackages = with pkgs; [ nixfmt virt-manager ];

  networking = {
    domain = "lan";
    hostName = "adomo-t14"; # Define your hostname.
    hostId = "81046d10";
  };

  programs = {
    adb.enable = true;
    bash.enableCompletion = true;
    gamescope.args = [ "--adaptive-sync" ];
    mosh.enable = true;
    ssh.startAgent = false;
  };
  services = {
    flatpak.enable = true;
    autorandr = {
      enable = false;
      defaultTarget = "home-prime";
    };
    journald.extraConfig = "SystemMaxUse=500M";
    atd.enable = true;
    fstrim.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    udev.extraRules = ''
      # Lenovo Lenovo Essential Wireless Keyboard and Mouse Combo
      ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", ATTRS{idVendor}=="17ef", ATTRS{idProduct}=="60a9", ATTR{power/wakeup}="disabled"
    '';

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
    hashedPasswordFile = config.sops.secrets."adomas/password".path;
  };
  users.users.root.hashedPasswordFile =
    config.sops.secrets."root/password".path;

  services = {
    xserver.libinput.enable = true;
    thinkfan = {
      sensors = [
        {
          type = "tpacpi";
          query = "/proc/acpi/ibm/thermal";
          indices = [ 0 ];
        }
        {
          name = "amdgpu";
          query = "/sys/class/hwmon";
          type = "hwmon";
          indices = [ 1 ];
        }
        {
          name = "nvme";
          query = "/sys/class/hwmon";
          type = "hwmon";
          indices = [ 1 ];
        }
      ];
    };
  };

  system.stateVersion = "22.05"; # Did you read the comment?

}
