{ pkgs, ... }:

{
  home.packages = with pkgs; [
    corefonts
    material-icons
    nerdfonts
    noto-fonts-emoji
  ];
}
