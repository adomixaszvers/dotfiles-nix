{ pkgs, inputs, ... }: {
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4
    ./dns.nix
    ./fail2ban.nix
    ../flakes.nix
    ./static-ip.nix
    ./syncthing.nix
    ./traefik.nix
    ./users.nix
    # ./vaultwarden.nix
    ./wireguard.nix
    ../zerotier.nix
    ./zsh.nix
  ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.raspberryPi = {
    enable = true;
    version = 4;
    firmwareConfig = ''
      dtoverlay=disable-wifi
      dtoverlay=disable-bt
    '';
  };

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
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    # Free up to 1GiB whenever there is less than 100MiB left.
    extraOptions = ''
      min-free = ${toString (100 * 1024 * 1024)}
      max-free = ${toString (1024 * 1024 * 1024)}
    '';
  };

  environment.systemPackages =
    [ pkgs.bind pkgs.git pkgs.wol pkgs.nixfmt pkgs.neovim pkgs.libraspberrypi ];
  networking = { hostName = "raspberrypi-nixos"; };
  services.openssh = {
    enable = true;
    passwordAuthentication = false;
  };
  virtualisation.podman.enable = true;
  virtualisation.oci-containers.backend = "podman";

  powerManagement.cpuFreqGovernor = "schedutil";

  system.stateVersion = "21.11";
}