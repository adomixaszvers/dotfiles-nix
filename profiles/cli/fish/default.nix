{
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./init.fish;
    shellAliases = {
      ls = "exa";
      la = "exa -a --group-directories-first";
      ll = "exa -al --group-directories-first";
      lt = "exa -aT";
    };
  };
}
