{
  home.file.".emacs.d/init.el" = {
    source = ./dotfiles/emacs.d/init.el;
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      counsel
      counsel-projectile
      evil
      evil-collection
      evil-leader
      evil-magit
      flycheck
      fzf
      ivy
      nix-mode
      swiper
      use-package
    ];
  };
  services.emacs.enable = true;
}
