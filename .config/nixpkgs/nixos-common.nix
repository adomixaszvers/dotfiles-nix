{ config, pkgs, ... }:
{
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    acpi
    git
    grml-zsh-config
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
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      source-code-pro
      google-fonts
      nerdfonts
      roboto
    ];
  };

  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.configFile = pkgs.runCommand "default.pa" {} ''
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

    # Make user colour green in prompt instead of default blue
    zstyle ':prompt:grml:left:items:user' pre '%F{green}%B'
  '';
  programs.zsh.promptInit = ""; # otherwise it'll override the grml prompt

  services.acpid.enable = true;
  services.nixosManual.showManual = true;

  services.xserver.enable = true;
  services.xserver.layout = "lt,us";
  services.xserver.displayManager.lightdm.enable = true;

  time.timeZone = "Europe/Vilnius";

  users.defaultUserShell = pkgs.zsh;
  users.mutableUsers = false;
  users.users.root.hashedPassword = "***REMOVED***";
  users.extraUsers.adomas = {
    hashedPassword = "***REMOVED***";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };
}
