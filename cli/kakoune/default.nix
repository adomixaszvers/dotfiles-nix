{ pkgs, ... }: {
  home.packages = with pkgs.channels.nixos-unstable; [
    (kakoune.override {
      configure.plugins = with kakounePlugins;
        [ kak-fzf ]
        ++ [ (callPackage (import ./kakoune-text-objects.nix) { }) ];
    })
    mine.kaknix
  ];
  xdg.configFile."kak" = {
    source = ./kak;
    recursive = true;
  };
}
