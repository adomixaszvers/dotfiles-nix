{ pkgs, ... }: {
  # TODO make it a proper module
  home.file.".emacs.d" = {
    source = pkgs.fetchFromGitHub {
      owner = "plexus";
      repo = "chemacs2";
      rev = "c2d700b784c793cc82131ef86323801b8d6e67bb";
      hash = "sha256-/WtacZPr45lurS0hv+W8UGzsXY3RujkU5oGGGqjqG0Q=";
    };
    # recursive = true;
  };
  home.file.".emacs-profiles.el".source = ./emacs-profiles.el;
}
