find-shells () {
  cat "$XDG_DATA_HOME"/direnv/allow/* | sort | uniq | sed -e 's/\.envrc$/shell.nix/'
}
hcd () {
  cd ~/.config/nixpkgs || exit 1
}
he () {
  (
    hcd && local FILES=("${(@f)$(fzf --multi)}") && [ -n "${FILES[*]}" ] && "$EDITOR" "${FILES[@]}"
  )
}
