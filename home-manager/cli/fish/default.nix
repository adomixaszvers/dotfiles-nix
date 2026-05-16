{ pkgs, ... }:
{
  programs.fish = {
    enable = true;
    interactiveShellInit = builtins.readFile ./init.fish;
    plugins = [
      {
        name = "pure";
        inherit (pkgs.fishPlugins.pure) src;
      }
    ];
  };
  stylix.targets.fish.enable = true;
}
