{
  home.file.".emacs.d/init.el" = {
    source = ./dotfiles/emacs.d/init.el;
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [ evil evil-collection nix-mode evil-magit ];
  };
}
