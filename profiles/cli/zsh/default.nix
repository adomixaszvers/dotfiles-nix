{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    plugins = [{
      name = "fz";
      src = pkgs.nivSources.fz;
    }];
    sessionVariables = { FZ_HISTORY_CD_CMD = "_zlua"; };
    shellAliases = {
      ls = "exa";
      la = "exa -a --group-directories-first";
      ll = "exa -al --group-directories-first";
      lt = "exa -aT";
      setlt = "setxkbmap lt,us -option grp:caps_toggle -model pc104";
    };
    initExtra = builtins.readFile ./initExtra.zsh;
  };
}
