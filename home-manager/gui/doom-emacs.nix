{ pkgs, ... }:
{
  home = {
    packages = builtins.attrValues {
      inherit (pkgs)
        # keep-sorted start
        emacs-all-the-icons-fonts
        fd
        gcc
        gnutls
        mdl
        multimarkdown
        ripgrep
        sqlite
        wordnet
        # keep-sorted end
        ;
    };
    sessionVariables.DOOMDIR = "~/.config/nixpkgs/home-manager/gui/doom";
  };
  programs.emacs.enable = true;
}
