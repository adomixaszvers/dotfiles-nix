{ pkgs, inputs, ... }:
let
  ePkgs = with inputs; import nixpkgs { overlays = [ emacs-overlay.overlay ]; };
in {
  home.packages = with pkgs; [
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
  ];
  home.file.".emacs-profile".text = ''
    doom
  '';
  programs.emacs = {
    enable = true;
    package = ePkgs.emacsGcc;
  };
}
