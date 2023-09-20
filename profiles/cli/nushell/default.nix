{ pkgs, ... }: {
  programs = {
    nushell = {
      enable = true;
      configFile.text = ''
        let carapace_completer = {|spans|
          ${pkgs.carapace}/bin/carapace $spans.0 nushell $spans | from json
        }

        alias hcd = cd ~/.config/nixpkgs

        def he [] {
          hcd
          let files = (fzf --multi | lines)
          if not ($files | is-empty) {
            run-external $env.EDITOR $files
          }
        }


        $env.config = {
          render_right_prompt_on_last_line: true
          show_banner: false
          edit_mode: vi
          completions: {
            external: {
              enable: true
              completer: $carapace_completer
            }
          }
        }
      '';
    };
    starship = {
      enable = true;
      enableZshIntegration = false;
      settings = builtins.fromTOML ''
        format = """
        $username\
        $hostname\
        $directory\
        $git_branch\
        $git_state\
        $git_status\
        $cmd_duration\
        $line_break\
        $python\
        $character"""

        [directory]
        style = "blue"

        [character]
        success_symbol = "[❯](purple)"
        error_symbol = "[❯](red)"
        vimcmd_symbol = "[❮](green)"

        [git_branch]
        format = "[$branch]($style)"
        style = "bright-black"

        [git_status]
        format = "[[(*$conflicted$untracked$modified$staged$renamed$deleted)](218) ($ahead_behind$stashed)]($style)"
        style = "cyan"
        conflicted = "​"
        untracked = "​"
        modified = "​"
        staged = "​"
        renamed = "​"
        deleted = "​"
        stashed = "≡"

        [git_state]
        format = '\([$state( $progress_current/$progress_total)]($style)\) '
        style = "bright-black"

        [cmd_duration]
        format = "[$duration]($style) "
        style = "yellow"

        [python]
        format = "[$virtualenv]($style) "
        style = "bright-black"

      '';
    };
    zoxide.enable = true;
  };
}
