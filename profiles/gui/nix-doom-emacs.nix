{ pkgs, ... }:

{
  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = ./doom;
  };
  home.packages = with pkgs; [
    fd
    gcc
    gnupg # for ghub .authinfo.gpg
    gnutls
    mdl
    multimarkdown
    nodePackages.prettier
    ripgrep
    sqlite
    wordnet
  ];
  home.file.".emacs-nix.d/init.el".text = ''
    (load "default.el")
  '';
  home.file.".emacs-profile".text = ''
    nix-doom
  '';
}
