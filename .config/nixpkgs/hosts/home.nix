{ pkgs, ... }:
{
  home.packages = with import ../packages.nix { inherit pkgs; }; common ++ home;
}
