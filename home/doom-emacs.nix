{ pkgs, ... }:
{
  home.packages = with pkgs; [ gcc gnutls pandoc fd ];
}
