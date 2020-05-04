{ pkgs, ... }: {
  home.packages = with pkgs; [
    (kakoune.override {
      configure.plugins = with kakounePlugins;
        [ kak-fzf ]
        ++ [ (callPackage (import ./kakoune-text-objects.nix) { }) ];
    })
    kak-lsp
    mine.kaknix
  ];
  xdg.configFile."kak" = {
    source = ./kak;
    recursive = true;
  };
  xdg.configFile."kak-lsp/kak-lsp.toml".source = ./kak-lsp.toml;
}
