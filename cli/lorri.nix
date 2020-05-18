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
      for i in $(fd -t f \\.envrc ~ --no-ignore-vcs --hidden -x echo {//}); do
        if [ -f $i/shell.nix ]; then
          echo "rebuilding $i"
          lorri watch --once --shell-file $i/shell.nix
        fi
      done
    }
  '';
  services.lorri.enable = true;
  xsession.importedVariables = [ "NIX_PATH" ];
}
