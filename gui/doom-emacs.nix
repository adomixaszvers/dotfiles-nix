{ pkgs, ... }: {
  home.packages = with pkgs; [
    emacs
    emacs-all-the-icons-fonts
    fd
    gcc
    gnupg # for gnub .authinfo.gpg
    gnutls
    mdl
    multimarkdown
    nodePackages.prettier
  ];
}
