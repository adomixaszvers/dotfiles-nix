{ pkgs, ... }: {
  home.packages = with pkgs; [
    emacs
    emacs-all-the-icons-fonts
    fd
    gcc
    gnutls
    mdl
    multimarkdown
    emacsPackagesNg.emacs-libvterm
    emacsPackagesNg.emacsql
  ];
}
