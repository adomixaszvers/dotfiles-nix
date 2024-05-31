{ lib, pkgs, ... }: {
  home.file.".zshrc".text = lib.mkBefore ''
    # fix hanging on emacs tramp
    # see: https://www.emacswiki.org/emacs/TrampMode#h5o-9
    [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
  '';
  home.packages = [ pkgs.pure-prompt ];
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    enableCompletion = true;
    defaultKeymap = "viins";
    plugins = [{
      name = "fz";
      src = pkgs.fetchFromGitHub {
        owner = "changyuheng";
        repo = "fz";
        rev = "fa41aa22abac86db1d1e9dd858d689d1c009aefa";
        hash = "sha256-M//pvh8WS0gsypfJzC1LRb7qdCxIdZWCgpIyh3/awh4=";
      };
    }];
    sessionVariables = { FZ_HISTORY_CD_CMD = "_zlua"; };
    shellAliases = {
      setlt = "setxkbmap lt,us -option grp:caps_toggle -model pc104";
    };
    initExtra = builtins.readFile ./initExtra.zsh;
  };
}
