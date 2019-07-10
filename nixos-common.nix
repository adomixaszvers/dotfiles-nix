{ config, pkgs, ... }: {
  nixpkgs.config.allowUnfree = true;

  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
      "https://nerdfonts.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
      "nerdfonts.cachix.org-1:aBl3vMJ8JpYEzuJYQ4OrfxGl5yoabTC7Tfh9fAcFf+c="
    ];
    extraOptions = ''
      keep-outputs = true
    '';
    optimise.automatic = true;
    trustedUsers = [ "root" "adomas" ];
  };

  documentation = {
    enable = true;
    dev.enable = true;
    info.enable = true;
  };
  environment.systemPackages = with pkgs; [
    acpi
    exfat
    git
    gparted
    lm_sensors
    neovim
    ntfs3g
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
  hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" { } ''
    sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
      ${pkgs.pulseaudioLight}/etc/pulse/default.pa > $out
  '';

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "lt_LT.UTF-8";
  };

  networking.networkmanager.enable = true;

  programs.command-not-found.enable = true;
  programs.zsh.enable = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.interactiveShellInit = ''
    source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
  '';
  programs.zsh.promptInit = ""; # otherwise it'll override the grml prompt

  services.acpid.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.nixosManual.showManual = true;

  services.gnome3.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  services.gnome3.seahorse.enable = true;

  services.xserver.enable = true;
  services.xserver.exportConfiguration = true;
  services.xserver.layout = "lt,us";
  services.xserver.displayManager.lightdm.enable = true;

  system.autoUpgrade = { enable = true; };

  time.timeZone = "Europe/Vilnius";

  users = let secrets = import ./secrets.nix;
  in {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
    users.root.hashedPassword = secrets.root.hashedPassword;
    extraUsers.adomas = {
      hashedPassword = secrets.adomas.hashedPassword;
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "wheel" ];
      shell = pkgs.zsh;
    };
  };
}
