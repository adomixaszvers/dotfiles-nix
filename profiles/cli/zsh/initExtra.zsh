find-shells () {
  cat "$XDG_DATA_HOME"/direnv/allow/* | sort | uniq | sed -e 's/\.envrc$/shell.nix/'
}

rebuild_shells () {
  while read i; do
    if [ -f "$i" ]; then
      echo "rebuilding $i"
      nix-shell "$i" --run true
    fi
  done <<< $(find-shells)
}

# enable completion menu
zstyle ':completion:*' menu select
zmodload zsh/complist
# enable edit command line
autoload edit-command-line; zle -N edit-command-line

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char
bindkey '^e' edit-command-line

autoload -U promptinit; promptinit
prompt pure
if [ "$TERM" = linux ]; then
  PURE_PROMPT_SYMBOL=">"
  PURE_PROMPT_VICMD_SYMBOL="<"
  PURE_GIT_DOWN_ARROW="↓"
  PURE_GIT_UP_ARROW="↑"
  PURE_GIT_STASH_SYMBOL="#"
fi
