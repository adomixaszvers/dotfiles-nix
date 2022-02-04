{ pkgs, ... }: {
 programs.zsh = {
    enable = true;
    syntaxHighlighting.enable = true;
    # interactiveShellInit = ''
    #   source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
    # '';
    # promptInit = ""; # otherwise it'll override the grml prompt
  };
}
