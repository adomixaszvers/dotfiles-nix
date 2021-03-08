{ pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = true;

  nix = {
    binaryCaches = [
      "https://pre-commit-hooks.cachix.org"
      "https://nix-community.cachix.org"
      "https://adomixaszvers.cachix.org"
    ];
    binaryCachePublicKeys = [
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "adomixaszvers.cachix.org-1:r3/lrlbDE7o/Vjk/muEU2iLIiCEZMbC09ZqiwAs64so="
    ];
    extraOptions = ''
      keep-outputs = true
    '';
    autoOptimiseStore = true;
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
    shared_mime_info
    usbutils
    wget
  ];
  environment.shells = [ pkgs.zsh ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    enableDefaultFonts = true;
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
  i18n = { defaultLocale = "lt_LT.UTF-8"; };

  networking.networkmanager.enable = lib.mkDefault true;

  programs.command-not-found.enable = true;
  programs.ssh.startAgent = true;
  programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
  };

  services.acpid.enable = true;
  services.colord.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  services.gnome3.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = lib.mkDefault true;
  programs.seahorse.enable = true;

  services.xserver = {
    enable = true;
    exportConfiguration = true;
    layout = "lt,us";
    desktopManager.xterm.enable = true;
    displayManager.lightdm.enable = lib.mkDefault true;
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
