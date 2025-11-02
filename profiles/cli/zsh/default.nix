{ lib, pkgs, ... }:
lib.mkMerge [
  {
    programs.zsh.initContent = lib.mkBefore ''
      # fix hanging on emacs tramp
      # see: https://www.emacswiki.org/emacs/TrampMode#h5o-9
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
    '';
  }
  {
    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;
      enableCompletion = true;
      defaultKeymap = "viins";
      plugins = [
        {
          name = "pure";
          src = "${pkgs.pure-prompt}/share/zsh/site-functions";
        }
        {
          name = "fz";
          src = pkgs.fetchFromGitHub {
            owner = "changyuheng";
            repo = "fz";
            rev = "fa41aa22abac86db1d1e9dd858d689d1c009aefa";
            hash = "sha256-M//pvh8WS0gsypfJzC1LRb7qdCxIdZWCgpIyh3/awh4=";
          };
        }
      ];
      sessionVariables = {
        FZ_HISTORY_CD_CMD = "_zlua";
      };
      shellAliases = {
        setlt = "setxkbmap lt,us -option grp:caps_toggle -model pc104";
      };
      initContent = builtins.readFile ./initExtra.zsh;
    };
  }
]
