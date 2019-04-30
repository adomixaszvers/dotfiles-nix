{ pkgs, ... }:
{
  home.file.".emacs.d" = {
    source = pkgs.stdenv.mkDerivation {
      name = "emacs.d";
      src = ./dotfiles/emacs.d;
      nativeBuildInputs = [ pkgs.emacs ];
      unpackPhase = ":";
      installPhase = ''
        mkdir $out
        cp -r $src/* $out
        emacs --batch --file $out/config.org --eval "(org-babel-tangle)"
      '';
    };
    recursive = true;
  };
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      beacon
      benchmark-init
      counsel
      counsel-projectile
      dashboard
      direnv
      doom-themes
      evil
      evil-collection
      evil-commentary
      evil-magit
      evil-org
      evil-surround
      flx
      flycheck
      format-all
      fzf
      geiser
      ivy
      ivy-hydra
      magit
      nix-mode
      nov
      org-bullets
      powerline
      racer
      rainbow-delimiters
      ripgrep
      rust-mode
      spaceline
      swiper
      use-package
      which-key
      yaml-mode
      zoom
    ];
  };
}
