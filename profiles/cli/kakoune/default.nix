{ pkgs, myPkgs, inputs, ... }: {
  home.packages = with pkgs; [ kak-lsp myPkgs.kaknix ];
  programs.kakoune = {
    enable = true;
    plugins = let
      inherit (pkgs) lib;
      inherit (pkgs.kakouneUtils) buildKakounePluginFrom2Nix;
      kakouneTextObjects = let input = inputs.kakoune-text-objects;
      in buildKakounePluginFrom2Nix {
        pname = "kakoune-text-objects";
        version = input.shortRev;
        src = input.outPath;
        meta = with lib; {
          description = "kakoune plugin providing extra text-objects";
          homepage = "https://github.com/Delapouite/kakoune-text-objects";
          license = licenses.mit;
          platform = platforms.all;
        };

      };
      sudoWrite = let input = inputs.kakoune-sudo-write;
      in buildKakounePluginFrom2Nix {
        pname = "kakoune-sudo-write";
        version = input.shortRev;
        src = input.outPath;
        meta = with lib; {
          description = "Write to files using 'sudo'";
          homepage = "https://github.com/occivink/kakoune-sudo-write";
          license = licenses.unlicense;
          platform = platforms.all;
        };

      };
    in [ pkgs.kakounePlugins.kak-fzf kakouneTextObjects sudoWrite ];
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
