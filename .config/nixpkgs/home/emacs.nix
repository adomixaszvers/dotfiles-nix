{
  home.file.".emacs.d/init.el" = {
    source = ./dotfiles/emacs.d/init.el;
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [ evil evil-collection evil-leader fzf ivy counsel counsel-projectile swiper nix-mode evil-magit ];
  };
  services.emacs.enable = true;
}
