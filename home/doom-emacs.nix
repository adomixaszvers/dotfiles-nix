{ pkgs, ... }:
{
  home.packages = with pkgs; [ emacs gcc gnutls pandoc fd ];
}
