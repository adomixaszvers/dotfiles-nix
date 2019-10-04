{ pkgs, ... }: {
  home.packages = with pkgs; [
    (kakoune.override {
      configure.plugins = with kakounePlugins; [
        kak-fzf
        kak-powerline
        kakoune-text-objects
      ];
    })
    mine.kaknix
  ];
  xdg.configFile."kak" = {
    source = ./kak;
    recursive = true;
  };
}
