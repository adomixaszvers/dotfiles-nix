{ pkgs, ... }: {
  home.packages = with pkgs; [
    (kakoune.override { configure.plugins = [ kakounePlugins.kak-fzf ]; })
    mine.kaknix
  ];
  xdg.configFile."kak" = {
    source = ./kak;
    recursive = true;
  };
}
