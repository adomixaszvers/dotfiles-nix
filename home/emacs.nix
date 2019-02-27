{ pkgs, ... }:
{
  home.file.".emacs.d" = {
    source = pkgs.stdenv.mkDerivation {
      name = "emacs.d";
      src = ./dotfiles/emacs.d;
      buildInputs = [ pkgs.emacs ];
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
      counsel
      counsel-projectile
      dashboard
      doom-themes
      evil
      evil-collection
      flycheck
      flx
      fzf
      ivy
      magit
      nix-mode
      powerline
      rainbow-delimiters
      swiper
      spaceline
      use-package
      which-key
    ];
  };
}
