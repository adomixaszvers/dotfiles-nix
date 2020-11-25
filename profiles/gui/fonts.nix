{ pkgs, ... }:

{
  home.packages = with pkgs; [
    corefonts
    google-fonts
    material-icons
    nerdfonts
    noto-fonts-emoji
  ];
}
