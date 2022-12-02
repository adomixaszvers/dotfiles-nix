{ pkgs, lib, inputs, ... }: {
  imports = [
    # ./vaultwarden.nix
    ../flakes.nix
    ../nix-registry.nix
    ../gc.nix
    ../zerotier.nix
    ./acme.nix
    ./dns.nix
    # ./fail2ban.nix
    ./nginx.nix
    ./static-ip.nix
    ./syncthing.nix
    ./users.nix
    ./prebuild-configs.nix
    # ./webdav.nix
    ./wireguard.nix
    ./zsh.nix
    ./dante.nix
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    inputs.sops-nix.nixosModules.sops
  ];

  nixpkgs.config.allowUnfree = true;

  boot.tmpOnTmpfs = true;
  # boot.loader.raspberryPi = {
  #   enable = true;
  #   version = 4;
  #   firmwareConfig = ''
  #     dtoverlay=disable-wifi
  #     dtoverlay=disable-bt
  #   '';
  # };

  programs.mosh.enable = true;

  services.udev.extraRules = ''
    SUBSYSTEM=="vchiq",KERNEL=="vchiq",GROUP="video",MODE="0660"
  '';

  # File systems configuration for using the installer's partition layout
  fileSystems = {
    # Prior to 19.09, the boot partition was hosted on the smaller first partition
    # Starting with 19.09, the /boot folder is on the main bigger partition.
    # The following is to be used only with older images.
    /* "/boot" = {
         device = "/dev/disk/by-label/NIXOS_BOOT";
         fsType = "vfat";
       };
    */
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
  };

  time.timeZone = "Europe/Vilnius";

  nix = {
    settings = {
      auto-optimise-store = true;
      substituters = lib.mkAfter [
        "https://pre-commit-hooks.cachix.org"
        "https://nix-community.cachix.org"
        "https://adomixaszvers.cachix.org"
      ];
      trusted-public-keys = [
        "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "adomixaszvers.cachix.org-1:r3/lrlbDE7o/Vjk/muEU2iLIiCEZMbC09ZqiwAs64so="
      ];
      # Free up to 1GiB whenever there is less than 100MiB left.
      keep-outputs = true;
      min-free = 100 * 1024 * 1024;
      max-free = 1024 * 1024 * 1024;
    };
  };

  environment.systemPackages = with pkgs; [
    bind
    git
    libraspberrypi
    lm_sensors
    neovim
    nixfmt
    wol
  ];
  networking = { hostName = "raspberrypi-nixos"; };
  services.fstrim.enable = true;
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  virtualisation.podman.enable = true;
  virtualisation.oci-containers.backend = "podman";

  powerManagement.cpuFreqGovernor = "schedutil";

  system.stateVersion = "22.05";
}
