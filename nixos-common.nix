{ config, pkgs, lib, ... }: {
  nixpkgs.config.allowUnfree = true;

  nix = {
    binaryCaches = [ "https://all-hies.cachix.org" ];
    binaryCachePublicKeys =
      [ "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k=" ];
    extraOptions = ''
      keep-outputs = true
    '';
    autoOptimiseStore = true;
  };

  documentation = {
    enable = true;
    dev.enable = true;
    info.enable = true;
  };
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
  hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" { } ''
    sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
      ${pkgs.pulseaudioLight}/etc/pulse/default.pa > $out
  '';

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
    interactiveShellInit = ''
      source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    '';
    promptInit = ""; # otherwise it'll override the grml prompt
  };

  services.acpid.enable = true;
  services.colord.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.nixosManual.showManual = true;

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

  users = let secrets = import ./secrets.nix;
  in {
    defaultUserShell = pkgs.zsh;
    mutableUsers = false;
    users.root.hashedPassword = secrets.root.hashedPassword;
    extraUsers.adomas = {
      hashedPassword = secrets.adomas.hashedPassword;
      isNormalUser = true;
      uid = 1000;
      extraGroups = [ "video" "wheel" ];
      shell = pkgs.zsh;
    };
  };
}
