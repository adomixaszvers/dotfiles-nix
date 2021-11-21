{
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./init.fish;
  };
}
