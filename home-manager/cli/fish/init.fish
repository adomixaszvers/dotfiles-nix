function hcd
    cd ~/.config/nixpkgs || exit 1
end

if [ "$TERM" = linux ]
  set -g pure_symbol_prompt ">"
  set -g pure_symbol_reverse_prompt "<"
  set -g pure_symbol_git_unpulled_commits "↓"
  set -g pure_symbol_git_unpushed_commits "↑"
  set -g pure_symbol_git_stash "#"
end
