{
  home.file.".emacs.d/init.el" = {
    source = ./dotfiles/emacs.d/init.el;
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      counsel
      counsel-projectile
      dashboard
      flycheck
      flx
      fzf
      ivy
      magit
      nix-mode
      swiper
      use-package
      which-key
    ];
  };
  services.emacs.enable = true;
}
