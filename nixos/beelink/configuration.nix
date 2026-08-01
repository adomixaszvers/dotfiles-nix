# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{
  pkgs,
  inputs,
  config,
  lib,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./static-ip.nix
    ../avahi.nix
    ../flakes.nix
    ../gc.nix
    # ../aarch64.nix
    # ../ld-link.nix
    ../nix-registry.nix
    ./acme.nix
    ./nginx.nix
    ./searx.nix
    ./adguard.nix
    ./forgejo.nix
    ./forgejo-runner.nix
    ./atuin.nix
    ./buildbot-master.nix
    ./buildbot-worker.nix
    # ./forgejo-runner.nix
    ../zerotier.nix
    ./wireguard.nix
    ./syncthing.nix
    ./home-manager.nix
    ./harmonia.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.common-gpu-intel
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "beelink"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Set your time zone.
  time.timeZone = "Europe/Vilnius";

  # Configure keymap in X11
  # services.xserver.xkb = {
  #   # layout = "lt";
  #   variant = "";
  # };

  # Configure console keymap
  # console.keyMap = "lt.baltic";

  sops.secrets = {
    "adomas/password" = {
      sopsFile = ./secrets/passwords.yaml;
      neededForUsers = true;
    };
    "root/password" = {
      sopsFile = ./secrets/passwords.yaml;
      neededForUsers = true;
    };
  };

  users.users = {
    # Define a user account. Don't forget to set a password with ‘passwd’.
    adomas = {
      isNormalUser = true;
      hashedPasswordFile = config.sops.secrets."adomas/password".path;
      description = "Adomas Jatuzis";
      extraGroups = [
        "networkmanager"
        "wheel"
      ];
      openssh.authorizedKeys.keyFiles = [
        ../keys/laptop.pub
        ../keys/juice_ed25519.pub
        ../keys/yubikey.pub
        ../keys/t14.pub
      ];
    };
    root.hashedPasswordFile = config.sops.secrets."root/password".path;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # keep-sorted start
    acpi
    efibootmgr
    exfat
    git
    lm_sensors
    neovim
    nixfmt
    ntfs3g
    pciutils
    psmisc
    shared-mime-info
    usbutils
    vim
    wget
    # keep-sorted end
  ];
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services = {
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    postgresql.package = pkgs.postgresql_18;
  };

  nix = {
    settings = {
      keep-outputs = true;
      substituters = lib.mkAfter [
        "https://nix-community.cachix.org"
        "https://cache.nixos-cuda.org"
        "https://adomixaszvers.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
        "adomixaszvers.cachix.org-1:r3/lrlbDE7o/Vjk/muEU2iLIiCEZMbC09ZqiwAs64so="
      ];
      auto-optimise-store = true;
    };
  };

  documentation = {
    enable = true;
  };
  environment.shells = [
    pkgs.zsh
    pkgs.nushell
  ];

  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LANGUAGE = "en_US.UTF-8";
      LC_ADDRESS = "lt_LT.UTF-8";
      LC_COLLATE = "lt_LT.UTF-8";
      LC_CTYPE = "lt_LT.UTF-8";
      LC_IDENTIFICATION = "lt_LT.UTF-8";
      LC_MEASUREMENT = "lt_LT.UTF-8";
      LC_MESSAGES = "lt_LT.UTF-8";
      LC_MONETARY = "lt_LT.UTF-8";
      LC_NAME = "lt_LT.UTF-8";
      LC_NUMERIC = "lt_LT.UTF-8";
      LC_PAPER = "lt_LT.UTF-8";
      LC_TELEPHONE = "lt_LT.UTF-8";
      LC_TIME = "lt_LT.UTF-8";
    };
  };

  networking = {
    # enableIPv6 = lib.mkDefault false;
    networkmanager = {
      unmanaged = [
        "driver:wireguard"
        "interface-name:br-*"
        "interface-name:docker*"
        "interface-name:virbr*"
      ];
    };
  };

  programs = {
    command-not-found.enable = true;
    iotop.enable = true;
    ssh.startAgent = false;
    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
    };
  };

  services = {
    acpid.enable = true;
    colord.enable = true;
  };

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = lib.mkDefault false;
    extraUsers.adomas = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [
        "networkmanager"
        "video"
        "wheel"
      ];
    };
  };

  virtualisation.podman.enable = true;
  virtualisation.oci-containers.backend = "podman";

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.05"; # Did you read the comment?

}
