{ pkgs, ... }:

let
  doom-emacs = pkgs.callPackage pkgs.nivSources.nix-doom-emacs {
    # Directory containing your config.el init.el
    # and packages.el files
    doomPrivateDir = ./doom;
  };
in {
  home.packages = [ doom-emacs ] ++ (with pkgs; [
    emacs-all-the-icons-fonts
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
  ]);
  home.file.".emacs-nix.d/init.el".text = ''
    (load "default.el")
  '';
  home.file.".emacs-profile".text = ''
    nix-doom
  '';
}
