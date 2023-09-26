{ pkgs, ... }: {
  programs = {
    nushell = {
      enable = false;
      configFile.text = ''
        let carapace_completer = {|spans: list<string>|
            ${pkgs.carapace}/bin/carapace $spans.0 nushell $spans
            | from json
            | if ($in | default [] | where value =~ '^-.*ERR$' | is-empty) { $in } else { null }
        }
        let zoxide_completer = {|spans|
            $spans | skip 1 | zoxide query -l $in | lines | where {|x| $x != $env.PWD}
        }
        let fish_completer = {|spans|
            ${pkgs.fish}/bin/fish --command $'complete "--do-complete=($spans | str join " ")"'
            | $"value(char tab)description(char newline)" + $in
            | from tsv --flexible --no-infer
        }


        let external_completer = {|spans|
          let expanded_alias = (scope alias-completions
          | where name == $spans.0
          | get -i 0.expansion)

          let spans = (if $expanded_alias != null {
              $spans
              | skip 1
              | prepend ($expanded_alias | split row ' ')
          } else {
              $spans
          })

          match $spans.0 {
              z => $zoxide_completer
              zi => $zoxide_completer
              _ => $carapace_completer
          } | do $in $spans
        }

        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/git/git-completions.nu *
        use ${pkgs.nu_scripts}/share/nu_scripts/custom-completions/nix/nix-completions.nu *

        def "manpages" [] {

            ^man -w
          | str trim
            | split row (char esep)
          | par-each { glob $'($in)/man?' }
          | flatten
          | par-each { ls $in | get name }
            | flatten
          | path basename
          | str replace -s ".gz" ""
        }

        export extern "man" [
            ...targets: string@"manpages"
        ]

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
              completer: $external_completer
            }
          }
        }

        use ${pkgs.nu_scripts}/share/nu_scripts/themes/themes/nord.nu
        let-env config = ($env.config | merge {color_config: (nord)})
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
