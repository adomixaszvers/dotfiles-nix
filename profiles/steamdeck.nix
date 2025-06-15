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
    ./gui/stylix.nix
    ./gui/fonts.nix
    ./gui/kitty.nix
  ];
  home.packages = with pkgs; [
    # glibc_multi
    # firefox # does not detect amd gpu
    (config.lib.nixGL.wrap firefox)
    keepassxc
    xsel
  ];
  home.sessionVariables = {
    BROWSER = "firefox";
    SSH_AUTH_SOCK = "\${XDG_RUNTIME_DIR}/gnupg/S.gpg-agent.ssh";
  };
  manual.html.enable = true;
  nixGL = {
    defaultWrapper = "mesa";
    installScripts = [ "mesa" ];
    inherit (inputs.nixGL) packages;
  };
  nixCats.packageNames = [ "nixCats-small" ];
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
    kitty.package = config.lib.nixGL.wrap pkgs.kitty;
    ssh = {
      enable = true;
      extraConfig = ''
        PKCS11Provider "${pkgs.yubico-piv-tool}/lib/libykcs11.so"
      '';
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
  targets.genericLinux.enable = true;
}
