{
  nix.gc = {
    automatic = true;
    options = "-d --delete-older-than 14d";
  };
}
