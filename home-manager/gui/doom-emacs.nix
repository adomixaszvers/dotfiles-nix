{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      emacs-all-the-icons-fonts
      fd
      gcc
      gnutls
      mdl
      multimarkdown
      ripgrep
      sqlite
      wordnet
    ];
    sessionVariables.DOOMDIR = "~/.config/nixpkgs/profiles/gui/doom";
  };
  programs.emacs.enable = true;
}
