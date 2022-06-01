{ pkgs, ... }: {
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
    package = pkgs.nixFlakes;
  };
}
