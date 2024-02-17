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
    ../steam.nix
    ../kde.nix
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
    # kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    kernel.sysctl."vm.max_map_count" = 2147483642;
    zfs.requestEncryptionCredentials = false;
  };

  networking = {
    hostName = "adomo-pc-nixos"; # Define your hostname.
    hostId = "92b8e669";
    networkmanager.enable = true;
  };

  hardware = {
    bluetooth.enable = true;
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
    gamescope.args = [ "--adaptive-sync" "-W 1920 -H 1080" ];
    # nix flake check doesn't complain but nixos-rebuild does
    ssh.askPassword = "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  };

  services = {
    fwupd.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    xserver.enable = true;
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
    hashedPasswordFile = config.sops.secrets."adomas/password".path;
    openssh.authorizedKeys.keyFiles =
      [ ../keys/juice_ed25519.pub ../keys/yubikey.pub ../keys/t14.pub ];
  };

  users.users.root.hashedPasswordFile =
    config.sops.secrets."root/password".path;

  system.stateVersion = "22.11"; # Did you read the comment?

}
