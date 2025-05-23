{ pkgs, lib, ... }:
{
  nixpkgs.config.allowUnfree = true;

  nix = {
    settings = {
      keep-outputs = true;
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
      auto-optimise-store = true;
    };
  };

  documentation = {
    enable = true;
  };
  environment.systemPackages = with pkgs; [
    acpi
    efibootmgr
    exfat
    git
    vim
    gparted
    lm_sensors
    neovim
    nixfmt-rfc-style
    ntfs3g
    pciutils
    psmisc
    shared-mime-info
    usbutils
    wget
  ];
  environment.shells = [
    pkgs.zsh
    pkgs.nushell
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    enableDefaultPackages = true;
  };

  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      # LANGUAGE = "en_US:lt_LT:C";
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
    enableIPv6 = lib.mkDefault false;
    networkmanager = {
      enable = lib.mkDefault true;
      unmanaged = [
        "driver:wireguard"
        "interface-name:br-*"
        "interface-name:docker*"
        "interface-name:virbr*"
      ];
    };
  };
  systemd.services.NetworkManager-wait-online = {
    serviceConfig = {
      ExecStart = [
        ""
        "${pkgs.networkmanager}/bin/nm-online -q"
      ];
    };
  };

  programs = {
    command-not-found.enable = true;
    dconf.enable = true;
    iotop.enable = true;
    ssh.startAgent = false;
    seahorse.enable = true;
    zsh = {
      enable = true;
      syntaxHighlighting.enable = true;
    };
  };

  services = {
    acpid.enable = true;
    colord.enable = true;
    dbus.packages = with pkgs; [ dconf ];
    gnome.gnome-keyring.enable = lib.mkDefault true;
    udisks2.enable = true;
  };

  services = {
    xserver = {
      enable = true;
      exportConfiguration = true;
      xkb.layout = "lt,us";
      desktopManager.xterm.enable = true;
      displayManager.startx.enable = lib.mkDefault true;
    };
    libinput = {
      enable = true;
      mouse.middleEmulation = lib.mkDefault false;
    };
  };

  time.timeZone = "Europe/Vilnius";

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
}
