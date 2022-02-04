{ pkgs, ... }: {
  nix = {
    package = pkgs.nix_2_4;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
