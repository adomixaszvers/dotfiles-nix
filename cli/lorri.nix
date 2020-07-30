{ pkgs, ... }: {
  programs.direnv = {
    stdlib = ''
      use_nix() {
        eval "$(lorri direnv)"
      }
    '';
  };
  programs.zsh.initExtra = ''
    lorri_rebuild () {
      while read i; do
        if [ -f "$i" ]; then
          echo "rebuilding $i"
          lorri watch --once --shell-file "$i"
        fi
      done <<< $(find-shells)
    }
  '';
  services.lorri.enable = true;
}
