{ lib, inputs, ... }: {
  home.file.".zshrc".text = lib.mkBefore ''
    # fix hanging on emacs tramp
    # see: https://www.emacswiki.org/emacs/TrampMode#h5o-9
    [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
  '';
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    defaultKeymap = "viins";
    plugins = [{
      name = "fz";
      src = inputs.fz;
    }];
    sessionVariables = { FZ_HISTORY_CD_CMD = "_zlua"; };
    shellAliases = {
      setlt = "setxkbmap lt,us -option grp:caps_toggle -model pc104";
    };
    initExtra = builtins.readFile ./initExtra.zsh;
  };
}
