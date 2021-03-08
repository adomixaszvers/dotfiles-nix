{ pkgs, inputs, ... }: {
  home.packages = with pkgs;
    let
      kakouneTextObjects = callPackage (import ./kakoune-text-objects.nix) {
        kakoune-text-objects-source = inputs.kakoune-text-objects;
      };
      sudoWrite = callPackage (import ./sudo-write.nix) {
        kakoune-sudo-write-source = inputs.kakoune-sudo-write;
      };
      kakrc = runCommandNoCCLocal "kakrc" { } ''
        mkdir -p $out/share/kak/autoload
        cp ${./kakrc.kak} $out/share/kak/autoload/kakrc.kak
      '';
      myKakoune = wrapKakoune kakoune-unwrapped {
        configure.plugins =
          [ kakounePlugins.kak-fzf kakouneTextObjects sudoWrite kakrc ];
      };
    in [ myKakoune kak-lsp mine.kaknix ];
  xdg.configFile."kak-lsp/kak-lsp.toml".source = ./kak-lsp.toml;
}
