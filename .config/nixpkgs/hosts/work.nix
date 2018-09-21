{ pkgs, ... }:
{
  home.packages = with import ../packages.nix pkgs; common ++ work;
}
