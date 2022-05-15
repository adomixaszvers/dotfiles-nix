{ pkgs, ... }: {
  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    fd
    gcc
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
  home.sessionVariables.DOOMDIR = "~/.config/nixpkgs/profiles/gui/doom";
  programs.emacs.enable = true;
}
