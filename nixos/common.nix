{ pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = true;

  nix = {
    settings = {
      keep-outputs = true;
      substituters = lib.mkAfter [
        "https://pre-commit-hooks.cachix.org"
        "https://nix-community.cachix.org"
        "https://hyprland.cachix.org"
        "https://adomixaszvers.cachix.org"
      ];
      trusted-public-keys = [
        "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "adomixaszvers.cachix.org-1:r3/lrlbDE7o/Vjk/muEU2iLIiCEZMbC09ZqiwAs64so="
      ];
      auto-optimise-store = true;
    };
  };

  documentation = { enable = true; };
  environment.systemPackages = with pkgs; [
    acpi
    efibootmgr
    exfat
    git
    vim
    gparted
    iotop
    lm_sensors
    neovim
    ntfs3g
    pciutils
    psmisc
    shared-mime-info
    usbutils
    wget
  ];
  environment.shells = [ pkgs.zsh ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    enableDefaultFonts = true;
  };

  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  i18n = { defaultLocale = "lt_LT.UTF-8"; };

  networking.networkmanager = {
    enable = lib.mkDefault true;
    unmanaged = [
      "driver:wireguard"
      "interface-name:br-*"
      "interface-name:docker*"
      "interface-name:virbr*"
    ];
  };

  programs = {
    command-not-found.enable = true;
    dconf.enable = true;
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
    gnome.gnome-keyring.enable = true;
    udisks2.enable = true;
  };

  services.xserver = {
    enable = true;
    exportConfiguration = true;
    layout = "lt,us";
    desktopManager.xterm.enable = true;
    displayManager.startx.enable = lib.mkDefault true;
    libinput.mouse.middleEmulation = lib.mkDefault false;
  };

  time.timeZone = "Europe/Vilnius";

  users = {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
    extraUsers.adomas = {
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "networkmanager" "video" "wheel" ];
      shell = pkgs.zsh;
    };
  };
}
