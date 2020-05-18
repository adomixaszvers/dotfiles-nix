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
        if [ -f "$i"/shell.nix ]; then
          echo "rebuilding $i"
          lorri watch --once --shell-file "$i"/shell.nix
        fi
      done <<< $(bfs ~ -type f -name .envrc -printf '%h\n')
    }
  '';
  services.lorri.enable = true;
  xsession.importedVariables = [ "NIX_PATH" ];
}
