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

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "lt_LT.UTF-8";
  };

  networking.networkmanager.enable = lib.mkDefault true;

  programs.command-not-found.enable = true;
  programs.ssh.startAgent = true;
  programs.zsh.enable = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.interactiveShellInit = ''
    source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
  '';
  programs.zsh.promptInit = ""; # otherwise it'll override the grml prompt

  services.acpid.enable = true;
  services.colord.enable = true;
  services.dbus.packages = with pkgs; [ gnome3.dconf ];
  services.nixosManual.showManual = true;

  services.gnome3.gnome-keyring.enable = true;
  security.pam.services.lightdm.enableGnomeKeyring = true;
  programs.seahorse.enable = true;

  services.xserver = {
    enable = true;
    exportConfiguration = true;
    layout = "lt,us";
    displayManager.lightdm.enable = true;
    desktopManager.session = [{
      name = "home-manager";
      start = ''
        ${pkgs.stdenv.shell} $HOME/.xsession-hm &
        waitPID=$!
      '';
    }];
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
