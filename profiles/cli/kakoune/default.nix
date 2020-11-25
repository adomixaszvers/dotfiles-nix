{ pkgs, ... }: {
  home.packages = with pkgs;
    let
      unstable = nixos-unstable;
      kakouneTextObjects = callPackage (import ./kakoune-text-objects.nix) { };
      sudoWrite = callPackage (import ./sudo-write.nix) { };
      kakrc = runCommandNoCCLocal "kakrc" { } ''
        mkdir -p $out/share/kak/autoload
        cp ${./kakrc.kak} $out/share/kak/autoload/kakrc.kak
      '';
      myKakoune = wrapKakoune kakoune-unwrapped {
        configure.plugins =
          [ kakounePlugins.kak-fzf kakouneTextObjects sudoWrite kakrc ];
      };
    in [ myKakoune unstable.kak-lsp mine.kaknix ];
  xdg.configFile."kak-lsp/kak-lsp.toml".source = ./kak-lsp.toml;
}
