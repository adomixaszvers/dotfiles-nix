{ pkgs, ... }: {
  home.file.".emacs.d" = {
    source = pkgs.stdenv.mkDerivation {
      name = "emacs.d";
      src = ./dotfiles/emacs.d;
      nativeBuildInputs = [pkgs.emacs];
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
    extraPackages = epkgs:
    with epkgs; [
      beacon
      benchmark-init
      counsel
      counsel-projectile
      csv-mode
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
      haskell-mode
      hindent
      intero
      ivy
      ivy-hydra
      lua-mode
      magit
      nix-mode
      nov
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
