{ pkgs, ... }: {
  home = {
    packages = with pkgs; [
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
    file.".emacs-profile".text = ''
      doom
    '';
    sessionVariables.DOOMDIR = "~/.config/nixpkgs/profiles/gui/doom";
  };
  programs.emacs = {
    enable = true;
    # package = pkgs.emacs29-pgtk;
  };
}
