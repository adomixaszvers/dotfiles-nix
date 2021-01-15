find-shells () {
  cat $XDG_DATA_HOME/direnv/allow/* | sort | uniq | sed -e 's/\.envrc$/shell.nix/'
}
hcd () {
  cd ~/.config/nixpkgs
}
he () {
  (
    hcd && local FILES="$(fzf)" && [ ! -z "$FILES" ] && $EDITOR $FILES
  )
}
