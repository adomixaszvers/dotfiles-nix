{ pkgs, ... }:

let
  doom-emacs = pkgs.callPackage pkgs.nivSources.nix-doom-emacs {
    doomPrivateDir = pkgs.gitignoreSource
      ~/.config/doom; # Directory containing your config.el init.el
    # and packages.el files
  };
in {
  home.packages = [ doom-emacs ] ++ (with pkgs; [
    emacs-all-the-icons-fonts
    fd
    gcc
    gnupg # for gnub .authinfo.gpg
    gnutls
    mdl
    multimarkdown
    nodePackages.prettier
  ]);
  home.file.".emacs-nix.d/init.el".text = ''
    (load "default.el")
  '';
  home.file.".emacs-profile".text = ''
    nix-doom
  '';
}
