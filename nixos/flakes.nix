{
  nix = {
    extraOptions = # ini
      ''
        experimental-features = nix-command flakes
      '';
  };
}
