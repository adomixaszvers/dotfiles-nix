{ config, pkgs, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common.nix
    ../flakes.nix
    ../gc.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../realtime.nix
    ../syncthing.nix
    ../yubikey.nix
    ./wireguard-client.nix
    inputs.nixos-hardware.nixosModules.common-gpu-amd
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    supportedFilesystems = [ "zfs" ];
    # kernelPackages = pkgs.linuxPackages_xanmod;
    kernel.sysctl."vm.max_map_count" = 2147483642;
  };

  networking = {
    hostName = "adomo-pc-nixos"; # Define your hostname.
    hostId = "92b8e669";
    networkmanager.enable = true;
  };

  hardware = {
    opengl.enable = true;
    nvidia = {
      modesetting.enable = true;
      powerManagement.enable = true;
    };
    xone.enable = true;
  };

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  programs = {
    steam = {
      enable = true;
      gamescopeSession.enable = true;
      package = pkgs.steam.override {
        extraPkgs = pkgs:
          with pkgs; [
            xorg.libXcursor
            xorg.libXi
            xorg.libXinerama
            xorg.libXScrnSaver
            libpng
            # libpulseaudio
            libvorbis
            stdenv.cc.cc.lib
            libkrb5
            keyutils
            mangohud
          ];
      };
    };
    gamescope = { enable = true; };
    # nix flake check doesn't complain but nixos-rebuild does
    ssh.askPassword = "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  };

  services = {
    fwupd.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    xserver = {
      enable = true;
      displayManager.sddm = {
        enable = true;
        autoNumlock = true;
      };
      desktopManager.plasma5.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
  ];

  sops.secrets."adomas/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };
  sops.secrets."root/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };

  users.users.adomas = {
    passwordFile = config.sops.secrets."adomas/password".path;
    openssh.authorizedKeys.keyFiles =
      [ ../keys/juice_ed25519.pub ../keys/yubikey.pub ../keys/t14.pub ];
  };

  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  system.stateVersion = "22.11"; # Did you read the comment?

}
