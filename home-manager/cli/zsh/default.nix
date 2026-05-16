{
  lib,
  pkgs,
  config,
  ...
}:
lib.mkMerge [
  {
    programs.zsh.initContent = lib.mkBefore ''
      # fix hanging on emacs tramp
      # see: https://www.emacswiki.org/emacs/TrampMode#h5o-9
      [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
    '';
  }
  {
    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;
      enableCompletion = true;
      defaultKeymap = "viins";
      dotDir = "${config.xdg.configHome}/zsh";
      plugins = [
        {
          name = "pure";
          src =
            let
              version = "1.27.1";
              upstreamPackage = pkgs.pure-prompt;
              updatedPackage = upstreamPackage.overrideAttrs (prev: {
                inherit version;
                src = prev.src.override {
                  sha256 = "sha256-Fhk4nlVPS09oh0coLsBnjrKncQGE6cUEynzDO2Skiq8=";
                };
              });
              isUpdatedVersion = lib.versionAtLeast upstreamPackage.version version;
              pure-prompt = lib.warnIf isUpdatedVersion "pure-prompt was updated in nixpkgs" updatedPackage;
            in
            "${pure-prompt}/share/zsh/site-functions";
        }
        {
          name = "fz";
          src = pkgs.fetchFromGitHub {
            owner = "changyuheng";
            repo = "fz";
            rev = "fa41aa22abac86db1d1e9dd858d689d1c009aefa";
            hash = "sha256-M//pvh8WS0gsypfJzC1LRb7qdCxIdZWCgpIyh3/awh4=";
          };
        }
      ];
      sessionVariables = {
        FZ_HISTORY_CD_CMD = "_zlua";
      };
      shellAliases = {
        setlt = "setxkbmap lt,us -option grp:caps_toggle -model pc104";
      };
      initContent = builtins.readFile ./initExtra.zsh;
    };
  }
]
