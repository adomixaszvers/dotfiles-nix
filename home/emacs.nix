{ pkgs, ... }:
let
  unstable = import <nixos-unstable> { };
  emacs = unstable.emacs;
in {
  home.file.".emacs.d" = {
    source = pkgs.stdenv.mkDerivation {
      name = "emacs.d";
      src = ./dotfiles/emacs.d;
      nativeBuildInputs = [emacs];
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
    package = emacs;
    extraPackages = epkgs:
    with unstable.emacsPackagesNg; [
      beacon
      benchmark-init
      counsel
      counsel-projectile
      csv-mode
      dashboard
      delight
      diminish
      direnv
      doom-themes
      emojify
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
      haskell-mode
      hindent
      intero
      ivy
      ivy-hydra
      lispy
      lispyville
      lua-mode
      magit
      markdown-mode
      melpaStablePackages.cider
      nix-mode
      nov
      org-beautify-theme
      org-bullets
      powerline
      pretty-mode
      racer
      rainbow-delimiters
      ripgrep
      rust-mode
      spaceline
      swiper
      use-package
      volatile-highlights
      which-key
      yaml-mode
      zoom
    ];
  };
}
