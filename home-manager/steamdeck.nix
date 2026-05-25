{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:
{
  imports = [
    ./cli
    ./cli/jujutsu.nix
    ./gui/stylix.nix
    ./gui/firefox.nix
    ./gui/fonts.nix
    ./gui/kitty.nix
  ];
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    # glibc_multi
    keepassxc
    xsel
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
    SSH_AUTH_SOCK = "\${XDG_RUNTIME_DIR}/ssh-agent";
  };
  manual.html.enable = true;
  programs = {
    gpg.enable = lib.mkForce false;
    bash.profileExtra = ''
      NIX_PATHS=""
      NON_NIX_PATHS=""
      for p in $(echo $PATH| tr ':' ' '); do
        case $p in
          *nix*) NIX_PATHS="$NIX_PATHS''${NIX_PATHS:+:}$p"
          ;;
          *) NON_NIX_PATHS="$NON_NIX_PATHS''${NON_NIX_PATHS:+:}$p"
          ;;
        esac
      done;
      export PATH="$NON_NIX_PATHS:$NIX_PATHS"
    '';
    firefox = {
      enable = true;
      # firefox # does not detect amd gpu
      package = config.lib.nixGL.wrap pkgs.firefox;
    };
    kitty.package = config.lib.nixGL.wrap pkgs.kitty;
    ssh = {
      enable = true;
      enableDefaultConfig = false;
      settings."*".PKCS11Provider = "${pkgs.yubico-piv-tool}/lib/libykcs11.so";
    };
  };
  services = {
    gpg-agent.enable = true;
    syncthing = {
      enable = true;
      # tray.enable = true;
    };
  };
  stylix.enable = true;
  xdg = {
    enable = true;
  };
  targets.genericLinux = {
    enable = true;
    nixGL = {
      defaultWrapper = "mesa";
      installScripts = [ "mesa" ];
      inherit (inputs.nixGL) packages;
    };
  };
  wrappers = {
    neovim.enable = false;
    neovim-nix.enable = true;
  };
}
