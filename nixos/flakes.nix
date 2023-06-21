{
  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes repl-flake
    '';
  };
}
