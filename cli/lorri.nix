{ pkgs, ... }: {
  programs.direnv = {
    stdlib = ''
      use_nix() {
        eval "$(lorri direnv)"
      }
    '';
  };
  services.lorri.enable = true;
  xsession.importedVariables = [ "NIX_PATH" ];
}
