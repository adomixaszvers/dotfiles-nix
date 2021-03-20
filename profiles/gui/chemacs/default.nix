{ inputs, ... }: {
  # TODO make it a proper module
  home.file.".emacs.d" = {
    source = inputs.chemacs;
    # recursive = true;
  };
  home.file.".emacs-profiles.el".source = ./emacs-profiles.el;
}
