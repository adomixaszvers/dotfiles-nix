{ pkgs, ... }:
{
  programs = {
    nushell = {
      shellAliases.hcd = "cd ~/.config/nixpkgs";
      configFile.text = ''
        let zoxide_completer = {|spans|
            $spans | skip 1 | zoxide query -l $in | lines | where {|x| $x != $env.PWD}
        }
        let fish_completer = {|spans|
            ${pkgs.fish}/bin/fish --command $'complete "--do-complete=($spans | str join " ")"'
            | $"value(char tab)description(char newline)" + $in
            | from tsv --flexible --no-infer
        }
        let nix_completer = {|spans|
          let current_arg = ($spans | length| $in - 1)
          with-env { NIX_GET_COMPLETIONS: $current_arg } { $spans| skip 1| run-external --redirect-stdout nix $in }
          | lines
          | skip 1
          | parse "{value}\t{description}"
        }


        let external_completer = {|spans|
          # does not work with v.80
          # let expanded_alias = (scope alias-completions
          # | where name == $spans.0
          # | get -i 0.expansion)

          # let spans = (if $expanded_alias != null {
          #     $spans
          #     | skip 1
          #     | prepend ($expanded_alias | split row ' ')
          # } else {
          #     $spans
          # })

          match $spans.0 {
              z => $zoxide_completer
              zi => $zoxide_completer
              nix => $nix_completer
              _ => $fish_completer
          } | do $in $spans
        }

        def "manpages" [] {

            ^man -w
          | str trim
            | split row (char esep)
          | par-each { glob $'($in)/man?' }
          | flatten
          | par-each { ls $in | get name }
            | flatten
          | path basename
          | str replace ".gz" ""
        }

        export extern "man" [
            ...targets: string@"manpages"
        ]

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

        use ${pkgs.nu_scripts}/share/nu_scripts/themes/nu-themes/nord.nu
        $env.config.color_config = (nord)
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
        $nix_shell\
        $cmd_duration\
        $line_break\
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
      '';
    };
    zoxide.enable = true;
  };
}
