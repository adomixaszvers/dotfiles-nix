{ config, pkgs, inputs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common.nix
    ../flakes.nix
    ../gc.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    supportedFilesystems = [ "zfs" ];
    kernelPackages = pkgs.linuxPackages_xanmod;
  };

  networking = {
    hostName = "adomo-pc-nixos"; # Define your hostname.
    hostId = "92b8e669";
    networkmanager.enable = true;
  };

  hardware = {
    opengl.enable = true;
    xone.enable = true;
  };

  programs.ssh.askPassword =
    "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";
  programs.steam.enable = true;

  services = {
    fwupd.enable = true;
    openssh.enable = true;
    xserver = {
      enable = true;
      displayManager.sddm.enable = true;
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
      [ ../keys/juice_ed25519.pub ../keys/yubikey.pub ];
  };

  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  system.stateVersion = "22.11"; # Did you read the comment?

}
