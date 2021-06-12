{ pkgs, myPkgs, inputs, ... }: {
  home.packages = with pkgs; [ kak-lsp myPkgs.kaknix ];
  programs.kakoune = {
    enable = true;
    plugins = with pkgs;
      let
        kakouneTextObjects = callPackage (import ./kakoune-text-objects.nix) {
          kakoune-text-objects-source = inputs.kakoune-text-objects;
        };
        sudoWrite = callPackage (import ./sudo-write.nix) {
          kakoune-sudo-write-source = inputs.kakoune-sudo-write;
        };
      in [ kakounePlugins.kak-fzf kakouneTextObjects sudoWrite ];
    config = {
      tabStop = 4;
      scrollOff = {
        columns = 5;
        lines = 5;
      };
      hooks = [
        {
          name = "ModuleLoaded";
          once = true;
          option = "fzf-file";
          commands = ''
            set-option global fzf_file_command 'fd --type f'
            set-option global fzf_highlight_command 'bat'
          '';
        }
        {
          name = "WinSetOption";
          option = "filetype=(rust|python|haskell)";
          commands = ''
            lsp-enable-window
            lsp-auto-hover-enable
            lsp-auto-hover-insert-mode-disable
            set-option window lsp_hover_anchor true
            set-option window lsp_hover_max_lines 10
            set-face window DiagnosticError default+u
            set-face window DiagnosticWarning default+u
            map window user l :enter-user-mode<space>lsp<ret>
          '';
        }
        {
          name = "WinSetOption";
          option = "filetype=nix";
          commands = ''
            set-option window formatcmd 'nixfmt'
            set-option window lintcmd 'kaknix'
            set-option window tabstop 2
            lint
            hook window -group lint BufWritePost .*\.nix lint
          '';
        }
      ];
      keyMappings = [
        {
          mode = "user";
          key = "f";
          effect = ":fzf-mode<ret>";
        }
        {
          mode = "insert";
          key = "<c-t>";
          effect = "<a-;><gt>";
        }
        {
          mode = "insert";
          key = "<c-d>";
          effect = "<a-;><lt>";
        }
        {
          mode = "user";
          key = "n";
          effect = ":lint-next-message<ret>";
        }
        {
          mode = "user";
          key = "p";
          effect = ":lint-previous-message<ret>";
        }
        {
          mode = "user";
          key = "c";
          effect = ":comment-line<ret>";
        }
      ];
      numberLines = {
        enable = true;
        highlightCursor = true;
        relative = true;
      };
    };
    extraConfig = ''
      eval %sh{kak-lsp --kakoune -s $kak_session}
      alias global x write-quit
    '';
  };
  xdg.configFile."kak-lsp/kak-lsp.toml".source = ./kak-lsp.toml;
}
