{ pkgs, ... }: {
  home.packages = let unstable = import <nixos-unstable> { };
  in with unstable; [
    (kakoune.override { configure.plugins = [ kakounePlugins.kak-fzf ]; })
    pkgs.mine.kaknix
  ];
  xdg.configFile."kak" = {
    source = ./kak;
    recursive = true;
  };
}

