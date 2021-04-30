{
  programs.direnv = {
    stdlib = ''
      use_lorri() {
        eval "$(lorri direnv)"
      }
    '';
  };
  programs.zsh.initExtra = ''
    lorri_rebuild () {
      while read i; do
        if [ -f "$i" ]; then
          echo "rebuilding $i"
          nix-shell "$i" --run true && lorri watch --once --shell-file "$i"
        fi
      done <<< $(find-shells)
    }
  '';
  services.lorri.enable = true;
}
